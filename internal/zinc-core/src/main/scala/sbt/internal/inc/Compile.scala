/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt
package internal
package inc

import java.io.File
import scala.collection.mutable
import sbt.internal.inc.Analysis.{ LocalProduct, NonLocalProduct }
import xsbt.api.{ APIUtil, HashAPI, NameHashing }
import xsbti.api._
import xsbti.compile.{
  CompileAnalysis,
  DependencyChanges,
  ExternalHooks,
  IncOptions,
  MiniSetup,
  Output,
  SourceSource,
  ClassFileManager => XClassFileManager
}
import xsbti.{ Position, Problem, Severity, UseScope }
import sbt.util.{ InterfaceUtil, Logger }
import sbt.util.InterfaceUtil.jo2o
import java.io.File
import java.util

import xsbti.AnalysisCallback.PickleData

import scala.collection.JavaConverters._
import xsbti.api.DependencyContext
import xsbti.compile.analysis.ReadStamps
import JavaInterfaceUtil._
import sbt.internal.inc.JarUtils.ClassInJar

/**
 * Helper methods for running incremental compilation.  All this is responsible for is
 * adapting any xsbti.AnalysisCallback into one compatible with the [[sbt.internal.inc.Incremental]] class.
 */
object IncrementalCompile {

  /**
   * Runs the incremental compilation algorithm.
   *
   * @param sources The full set of input sources
   * @param lookup An instance of the `Lookup` that implements looking up both classpath elements
   *               and Analysis object instances by a binary class name.
   * @param compile The mechanism to run a single 'step' of compile, for ALL source files involved.
   * @param previous0 The previous dependency Analysis (or an empty one).
   * @param output The configured output directory/directory mapping for source files.
   * @param log Where all log messages should go
   * @param options Incremental compiler options (like name hashing vs. not).
   * @return A flag of whether or not compilation completed successfully, and the resulting
   *         dependency analysis object.
   */
  def apply(
      sources: Set[File],
      lookup: Lookup,
      compile: (Set[File], DependencyChanges, xsbti.AnalysisCallback, XClassFileManager) => Unit,
      previous0: CompileAnalysis,
      output: Output,
      log: Logger,
      options: IncOptions,
      currentSetup: MiniSetup,
      outputJarContent: JarUtils.OutputJarContent
  ): (Boolean, Analysis) = {
    val previous = previous0 match { case a: Analysis => a }
    val externalHooks = options.externalHooks()
    val current =
      Stamps.initial(
        Stamper.forLastModified,
        Stamper.forHash(Hooks.sourceSource(externalHooks)),
        Stamper.forLastModified
      )
    val internalBinaryToSourceClassName = (binaryClassName: String) =>
      previous.relations.productClassName.reverse(binaryClassName).headOption
    val internalSourceToClassNamesMap: File => Set[String] = (f: File) =>
      previous.relations.classNames(f)
    val externalAPI = getExternalAPI(externalHooks, lookup)
    val profiler = Hooks.getInvalidationProfiler(externalHooks)
    try {
      Incremental.compile(
        sources,
        lookup,
        previous,
        current,
        compile,
        new AnalysisCallback.Builder(
          sources,
          internalBinaryToSourceClassName,
          internalSourceToClassNamesMap,
          externalAPI,
          current,
          output,
          options,
          currentSetup,
          outputJarContent,
          log
        ),
        log,
        options,
        output,
        outputJarContent,
        profiler
      )
    } catch {
      case _: xsbti.CompileCancelled =>
        log.info("Compilation has been cancelled")
        // in case compilation got cancelled potential partial compilation results (e.g. produced classs files) got rolled back
        // and we can report back as there was no change (false) and return a previous Analysis which is still up-to-date
        (false, previous)
    }
  }

/*
  // Skeletal implementation of a `quickAPI` callback,
  // making use of the `provenance` field composed of name.hash:

      // Build from whatever you're using for for PerClasspathEntryLookup
      private def analysisCache(name: String ): Option[CompileAnalysis] =  ???

      // Populate this during analysis read-mapping
      private val unchangedProvenances: Set[String] = ???

      private val nameToAnalysis: Map[String, Path] = ???
      private object ProvenanceExtractor {
         // Extract name and hash from name of the jar file where a class was found by scalac
         def unapply(file: File): Option[(String, String)] = ???
      }

      private val prevApis = prevAnalysis.asInstanceOf[Analysis].apis.external
      private val unknown = Optional.of(APIs.emptyAnalyzedClass)
      private val unavailable = Optional.empty[AnalyzedClass]
      private val quickAPICache = new mutable.HashMap[(File, String), Optional[AnalyzedClass]]

        private def quickAPI(fileWhereFound: File, fqcn: String): Optional[AnalyzedClass] =
          quickAPICache.getOrElseUpdate((fileWhereFound, fqcn), {

            def lookInUpstreamAnalysis(name: String): Option[AnalyzedClass] =
              // This horror is cribbed from s.i.i.IncrementalCompile
              for (analysis0 <- analysisCache(path);
                   analysis = analysis0 match { case a: Analysis => a };
                   sourceClassName <- analysis.relations.productClassName.reverse(fqcn).headOption;
                   api <- analysis.apis.internal.get(sourceClassName)) yield api

            fileWhereFound match {
              // We have no idea where the fqcn might be found, so start by looking in our previous analysis.
              case null =>
                prevApis.get(fqcn) match {
                  // If an api was stored with a provenance with an unchanged hash, we can return that api with confidence.
                  case Some(api) if unchanged(api.provenance()) =>
                    Optional.of(api)

                  // We know about this API, but its hash has been updated.  Look for the analysis of the new classpath
                  // entry. If we can't find the fqcn in the new location, then zinc will have to search for it.
                  case Some(api) if !api.provenance().isEmpty =>
                    val name = api.provenance().substring(0, api.provenance().lastIndexOf('.'))
                    lookInUpstreamAnalysis(name).fold(unknown)(Optional.of)

                  // We know nothing about this class.
                  case _ =>
                    unknown
                }

              // Class was found in our project, and we know where its analysis ought to live.  If we don't find the
              // fqcn there, it's not going to be available anywhere, so we return definitively unavailable rather
              // than unknown as we did above
              case ProvenanceExtractor(hame, hash) =>
                if (unchanged(hash))
                  prevApis.get(fqcn).fold(unavailable)(Optional.of)
                else
                  lookInUpstreamAnalysis(name).fold(unavailable)(Optional.of)

              // Either not on the classpath at all (which will eventually be a compilation error) or
              // in some library outside of the project.
              case _ =>
                unavailable
            }
          }
        )
*/

  def getExternalAPI(
      hooks: ExternalHooks,
      lookup: Lookup
  ): (File, String) => Option[AnalyzedClass] = {
    val quickAPI: (File, String) => Option[AnalyzedClass] = Hooks.quickAPI(hooks)
    (from: File, binaryClassName: String) => {
      quickAPI(from, binaryClassName) match {
        case None =>
          // Not even in project.  No API possible
          None
        case Some(api) if !api.provenance().isEmpty =>
          // Found it, in a good location.
          Some(api)
        case _ =>
          // Didn't find an API, but it still might be in the project somewhere.  We'll need to load all the
          // analyses to be sure.
          getExternalAPI0(lookup)(file, binaryClassName)
      }
    }
  }

  private def getExternalAPI0(lookup: Lookup): (File, String) => Option[AnalyzedClass] =
    (_: File, binaryClassName: String) =>
      lookup.lookupAnalysis(binaryClassName) flatMap {
        case (analysis: Analysis) =>
          val sourceClassName =
            analysis.relations.productClassName.reverse(binaryClassName).headOption
          sourceClassName flatMap analysis.apis.internal.get
      }
}

private object AnalysisCallback {

  /** Allow creating new callback instance to be used in each compile iteration */
  class Builder(
      allSources: Set[File],
      internalBinaryToSourceClassName: String => Option[String],
      internalSourceToClassNamesMap: File => Set[String],
      externalAPI: (File, String) => Option[AnalyzedClass],
      current: ReadStamps,
      output: Output,
      options: IncOptions,
      currentSetup: MiniSetup,
      outputJarContent: JarUtils.OutputJarContent,
      log: Logger
  ) {
    private val accruedPickleData = new mutable.ArrayBuffer[PickleData]
    def build(processAnalysis: ProcessAnalysis): AnalysisCallback = {
      new AnalysisCallback(
        allSources,
        processAnalysis,
        internalBinaryToSourceClassName,
        internalSourceToClassNamesMap,
        externalAPI,
        current,
        output,
        options,
        currentSetup,
        outputJarContent,
        accruedPickleData,
        log
      )
    }
  }
}

private final class AnalysisCallback(
    allSources: Set[File],
    process: ProcessAnalysis,
    internalBinaryToSourceClassName: String => Option[String],
    internalSourceToClassNamesMap: File => Set[String],
    externalAPI: (File, String) => Option[AnalyzedClass],
    stampReader: ReadStamps,
    output: Output,
    options: IncOptions,
    currentSetup: MiniSetup,
    outputJarContent: JarUtils.OutputJarContent,
    private val accruedPickleData: mutable.ArrayBuffer[PickleData],
    log: Logger
) extends xsbti.AnalysisCallback {

  private[this] val stringInterner = new Interner[String]

  private[this] val compilation: Compilation = Compilation(output)

  private val hooks = options.externalHooks()

  private val provenance =
    output.getSingleOutput.toOption.map(Hooks.getProvenance(hooks)(_)).getOrElse("").intern

  def isCanceled = hooks.isCanceled

  private val advancePhaseHook = Hooks.getAdvancePhase(hooks)

  override def advancePhase(prev: String, next: String): Unit = advancePhaseHook(prev, next)

  def inCompilation(file: File) = allSources.contains(file)

  override def toString =
    (List("Class APIs", "Object APIs", "Binary deps", "Products", "Source deps") zip
      List(classApis, objectApis, binaryDeps, nonLocalClasses, intSrcDeps))
      .map { case (label, map) => label + "\n\t" + map.mkString("\n\t") }
      .mkString("\n")

  import scala.collection.mutable.{ HashMap, HashSet, ListBuffer, Map, Set }

  final case class ApiInfo(
      publicHash: HashAPI.Hash,
      extraHash: HashAPI.Hash,
      classLike: ClassLike
  )

  import java.util.concurrent.{ ConcurrentLinkedQueue, ConcurrentHashMap }
  import scala.collection.concurrent.TrieMap

  private type ConcurrentSet[A] = ConcurrentHashMap.KeySetView[A, java.lang.Boolean]

  private[this] val srcs = ConcurrentHashMap.newKeySet[File]()
  private[this] val classApis = new TrieMap[String, ApiInfo]
  private[this] val objectApis = new TrieMap[String, ApiInfo]
  private[this] val classPublicNameHashes = new TrieMap[String, Array[NameHash]]
  private[this] val objectPublicNameHashes = new TrieMap[String, Array[NameHash]]
  private[this] val usedNames = new TrieMap[String, ConcurrentSet[UsedName]]
  private[this] val unreporteds = new TrieMap[File, ConcurrentLinkedQueue[Problem]]
  private[this] val reporteds = new TrieMap[File, ConcurrentLinkedQueue[Problem]]
  private[this] val mainClasses = new TrieMap[File, ConcurrentLinkedQueue[String]]
  private[this] val binaryDeps = new TrieMap[File, ConcurrentSet[File]]

  // source file to set of generated (class file, binary class name); only non local classes are stored here
  private[this] val nonLocalClasses = new TrieMap[File, ConcurrentSet[(File, String)]]
  private[this] val localClasses = new TrieMap[File, ConcurrentSet[File]]
  // mapping between src class name and binary (flat) class name for classes generated from src file
  private[this] val classNames = new TrieMap[File, ConcurrentSet[(String, String)]]
  // generated class file to its source class name
  private[this] val classToSource = new TrieMap[File, String]
  // internal source dependencies
  private[this] val intSrcDeps = new TrieMap[String, ConcurrentSet[InternalDependency]]
  // external source dependencies
  private[this] val extSrcDeps = new TrieMap[String, ConcurrentSet[ExternalDependency]]
  private[this] val binaryClassName = new TrieMap[File, String]
  // source files containing a macro def.
  private[this] val macroClasses = Set[String]()

  private[this] var savedPickles = false

  // Results of invalidation calculations (including whether to continue cycles)
  // the analysis at this point is not useful and so isn't included.
  private[this] var invalidationResults: Option[CompileCycleResults] = None

  private def add[A, B](map: TrieMap[A, ConcurrentSet[B]], a: A, b: B): Unit = {
    map.getOrElseUpdate(a, ConcurrentHashMap.newKeySet[B]()).add(b)
    ()
  }

  def startSource(source: File): Unit = {
    if (options.strictMode()) {
      assert(
        !srcs.contains(source),
        s"The startSource can be called only once per source file: $source"
      )
    }
    srcs.add(source)
    ()
  }

  def problem(
      category: String,
      pos: Position,
      msg: String,
      severity: Severity,
      reported: Boolean
  ): Unit = {
    for (source <- jo2o(pos.sourceFile)) {
      val map = if (reported) reporteds else unreporteds
      map
        .getOrElseUpdate(source, new ConcurrentLinkedQueue)
        .add(InterfaceUtil.problem(category, pos, msg, severity, None))
    }
  }

  def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext) = {
    if (onClassName != sourceClassName)
      add(intSrcDeps, sourceClassName, InternalDependency.of(sourceClassName, onClassName, context))
  }

  private[this] def externalBinaryDependency(
      binary: File,
      className: String,
      source: File,
      context: DependencyContext
  ): Unit = {
    binaryClassName.put(binary, className)
    add(binaryDeps, source, binary)
  }

  private[this] def externalSourceDependency(
      sourceClassName: String,
      targetBinaryClassName: String,
      targetClass: AnalyzedClass,
      context: DependencyContext
  ): Unit = {
    val dependency =
      ExternalDependency.of(sourceClassName, targetBinaryClassName, targetClass, context)
    add(extSrcDeps, sourceClassName, dependency)
  }

  def binaryDependency(
      classFile: File,
      onBinaryClassName: String,
      fromClassName: String,
      fromSourceFile: File,
      context: DependencyContext
  ) =
    internalBinaryToSourceClassName(onBinaryClassName) match {
      case Some(dependsOn) => // dependsOn is a source class name
        // dependency is a product of a source not included in this compilation
        classDependency(dependsOn, fromClassName, context)
      case None =>
        classToSource.get(classFile) match {
          case Some(dependsOn) =>
            // dependency is a product of a source in this compilation step,
            //  but not in the same compiler run (as in javac v. scalac)
            classDependency(dependsOn, fromClassName, context)
          case None =>
            externalDependency(classFile, onBinaryClassName, fromClassName, fromSourceFile, context)
        }
    }

  private[this] def externalDependency(
      classFile: File,
      onBinaryName: String,
      sourceClassName: String,
      sourceFile: File,
      context: DependencyContext
  ): Unit =
    externalAPI(classFile, onBinaryName) match {
      case Some(api) =>
        // dependency is a product of a source in another project
        val targetBinaryClassName = onBinaryName
        externalSourceDependency(sourceClassName, targetBinaryClassName, api, context)
      case None =>
        // dependency is some other binary on the classpath
        externalBinaryDependency(classFile, onBinaryName, sourceFile, context)
    }

  def generatedNonLocalClass(
      source: File,
      classFile: File,
      binaryClassName: String,
      srcClassName: String
  ): Unit = {
    //println(s"Generated non local class ${source}, ${classFile}, ${binaryClassName}, ${srcClassName}")
    add(nonLocalClasses, source, (classFile, binaryClassName))
    add(classNames, source, (srcClassName, binaryClassName))
    classToSource.put(classFile, srcClassName)
    ()
  }

  def generatedLocalClass(source: File, classFile: File): Unit = {
    //println(s"Generated local class ${source}, ${classFile}")
    add(localClasses, source, classFile)
    ()
  }

  def api(sourceFile: File, classApi: ClassLike): Unit = {
    import xsbt.api.{ APIUtil, HashAPI }
    val className = stringInterner(classApi.name)
    if (APIUtil.isScalaSourceName(sourceFile.getName) && APIUtil.hasMacro(classApi))
      macroClasses.add(className)
    val shouldMinimize = !Incremental.apiDebug(options)
    val savedClassApi = if (shouldMinimize) APIUtil.minimize(classApi) else classApi
    val apiHash: HashAPI.Hash = HashAPI(classApi)
    val nameHashes = (new xsbt.api.NameHashing(options.useOptimizedSealed())).nameHashes(classApi)
    classApi.definitionType match {
      case d @ (DefinitionType.ClassDef | DefinitionType.Trait) =>
        val extraApiHash = {
          if (d != DefinitionType.Trait) apiHash
          else HashAPI(_.hashAPI(classApi), includePrivateDefsInTrait = true)
        }

        classApis(className) = ApiInfo(apiHash, extraApiHash, savedClassApi)
        classPublicNameHashes(className) = nameHashes
      case DefinitionType.Module | DefinitionType.PackageModule =>
        objectApis(className) = ApiInfo(apiHash, apiHash, savedClassApi)
        objectPublicNameHashes(className) = nameHashes
    }
  }

  def mainClass(sourceFile: File, className: String): Unit = {
    mainClasses.getOrElseUpdate(sourceFile, new ConcurrentLinkedQueue).add(className)
    ()
  }

  def usedName(className: String, name: String, useScopes: util.EnumSet[UseScope]) =
    add(usedNames, stringInterner(className), UsedName(stringInterner(name), useScopes))

  override def enabled(): Boolean = options.enabled

  private[this] var got = false
  def getFinal: CompileCycleResults = {
    assert(!got, "Can't get final analysis callback results more than once.")
    got = true
    outputJarContent.scalacRunCompleted()
    val a = analysis
    if (invalidationResults.isEmpty)
      writeEarlyArtifacts(process.previousAnalysisPruned)
    process.completeCycle(invalidationResults, a)
  }

  private def analysis = addUsedNames(addCompilation(addProductsAndDeps(Analysis.empty)))

  def getOrNil[A, B](m: collection.Map[A, Seq[B]], a: A): Seq[B] = m.get(a).toList.flatten
  def addCompilation(base: Analysis): Analysis =
    base.copy(compilations = base.compilations.add(compilation))
  def addUsedNames(base: Analysis): Analysis = usedNames.foldLeft(base) {
    case (a, (className, names)) =>
      import scala.collection.JavaConverters._
      names.asScala.foldLeft(a) {
        case (a, name) => a.copy(relations = a.relations.addUsedName(className, name))
      }
  }

  private def companionsWithHash(className: String): (Companions, HashAPI.Hash, HashAPI.Hash) = {
    val emptyHash = -1
    val emptyClass =
      ApiInfo(emptyHash, emptyHash, APIUtil.emptyClassLike(className, DefinitionType.ClassDef))
    val emptyObject =
      ApiInfo(emptyHash, emptyHash, APIUtil.emptyClassLike(className, DefinitionType.Module))
    val ApiInfo(classApiHash, classHashExtra, classApi) = classApis.getOrElse(className, emptyClass)
    val ApiInfo(objectApiHash, objectHashExtra, objectApi) =
      objectApis.getOrElse(className, emptyObject)
    val companions = Companions.of(classApi, objectApi)
    val apiHash = (classApiHash, objectApiHash).hashCode
    val extraHash = (classHashExtra, objectHashExtra).hashCode
    (companions, apiHash, extraHash)
  }

  private def nameHashesForCompanions(className: String): Array[NameHash] = {
    val classNameHashes = classPublicNameHashes.get(className)
    val objectNameHashes = objectPublicNameHashes.get(className)
    (classNameHashes, objectNameHashes) match {
      case (Some(nm1), Some(nm2)) =>
        NameHashing.merge(nm1, nm2)
      case (Some(nm), None) => nm
      case (None, Some(nm)) => nm
      case (None, None)     => sys.error("Failed to find name hashes for " + className)
    }
  }

  private def analyzeClass(name: String): AnalyzedClass = {
    val hasMacro: Boolean = macroClasses.contains(name)
    val (companions, apiHash, extraHash) = companionsWithHash(name)
    val nameHashes = nameHashesForCompanions(name)
    val safeCompanions = SafeLazyProxy(companions).asInstanceOf[Lazy[Companions]]
    AnalyzedClass.of(
      compilation.getStartTime(),
      name,
      safeCompanions,
      apiHash,
      nameHashes,
      hasMacro,
      extraHash,
      provenance
    )
  }

  def createStamperForProducts(): File => xsbti.compile.analysis.Stamp = {
    JarUtils.getOutputJar(output) match {
      case Some(outputJar) => Stamper.forLastModifiedInJar(outputJar)
      case None            => stampReader.product _
    }
  }

  def addProductsAndDeps(base: Analysis): Analysis = {
    import scala.collection.JavaConverters._
    val stampProduct = createStamperForProducts()
    srcs.asScala.foldLeft(base) {
      case (a, src) =>
        val stamp = stampReader.source(src)
        val classesInSrc = classNames
          .getOrElse(src, ConcurrentHashMap.newKeySet[(String, String)]())
          .asScala
          .map(_._1)
        val analyzedApis = classesInSrc.map(analyzeClass)
        val info = SourceInfos.makeInfo(
          getOrNil(reporteds.mapValues { _.asScala.toSeq }, src),
          getOrNil(unreporteds.mapValues { _.asScala.toSeq }, src),
          getOrNil(mainClasses.mapValues { _.asScala.toSeq }, src)
        )
        val binaries = binaryDeps.getOrElse(src, ConcurrentHashMap.newKeySet[File]).asScala
        val localProds = localClasses
          .getOrElse(src, ConcurrentHashMap.newKeySet[File]())
          .asScala map { classFile =>
          val classFileStamp = stampProduct(classFile)
          LocalProduct(classFile, classFileStamp)
        }
        val binaryToSrcClassName =
          (classNames.getOrElse(src, ConcurrentHashMap.newKeySet[(String, String)]()).asScala map {
            case (srcClassName, binaryClassName) => (binaryClassName, srcClassName)
          }).toMap
        val nonLocalProds = nonLocalClasses
          .getOrElse(src, ConcurrentHashMap.newKeySet[(File, String)]())
          .asScala map {
          case (classFile, binaryClassName) =>
            val srcClassName = binaryToSrcClassName(binaryClassName)
            val classFileStamp = stampProduct(classFile)
            NonLocalProduct(srcClassName, binaryClassName, classFile, classFileStamp)
        }

        val internalDeps = classesInSrc.flatMap(
          cls =>
            intSrcDeps.getOrElse(cls, ConcurrentHashMap.newKeySet[InternalDependency]()).asScala
        )
        val externalDeps = classesInSrc.flatMap(
          cls =>
            extSrcDeps.getOrElse(cls, ConcurrentHashMap.newKeySet[ExternalDependency]()).asScala
        )
        val binDeps = binaries.map(d => (d, binaryClassName(d), stampReader binary d))

        a.addSource(
          src,
          analyzedApis,
          stamp,
          info,
          nonLocalProds,
          localProds,
          internalDeps,
          externalDeps,
          binDeps
        )
    }
  }

  private def writeEarlyArtifacts(merged: Analysis): Unit = {
    hooks.storeEarlyAnalysis(merged, currentSetup)
    val picklePath = hooks.pickleJarPath().toOption
    for (pickleJar <- picklePath) {
      // List classes defined in the files that were compiled in this run.
      val knownClasses = merged.relations.allSources
        .flatMap(merged.relations.products)
        .map(ClassInJar.fromFile(_).toClassFilePath)
      PickleJar.write(pickleJar, accruedPickleData, knownClasses, log)
      hooks.picklesComplete()
    }
  }

  override def dependencyPhaseCompleted(): Unit = {
    if (invalidationResults.isEmpty) {
      val CompileCycleResults(continue, invalidations, merged) =
        process.mergeAndInvalidate(analysis, false)
      // Store invalidations and continuation decision; the analysis will be computed again after Analyze phase.
      invalidationResults = Some(CompileCycleResults(continue, invalidations, Analysis.empty))
      // If there will be no more compilation cycles, store the early analysis file and update the pickle jar
      if (!continue)
        writeEarlyArtifacts(merged)
    }
    outputJarContent.dependencyPhaseCompleted()
  }

  override def apiPhaseCompleted(): Unit = {
    // If we know we're done with cycles (presumably because all sources were invalidated) we can store early analysis
    // and picke data now.  Otherwise, we need to wait for dependency information to decide if there are more cycles.
    if (process.isKnownFinal) {
     val CompileCycleResults(continue, invalidations, merged) = process.mergeAndInvalidate(analysis, false)
      assert(!continue && invalidations.isEmpty, "Everything was supposed to be invalidated already.")
      invalidationResults = Some(CompileCycleResults.empty)
      writeEarlyArtifacts(merged)
    }
  }

  // Can't do anything involving `global` here, because we probably don't have the
  // same classloader as the bridge.
  override def processPickleData(pickleData: Array[PickleData]): Unit = {
    assert(!savedPickles, "Pickle processing should only occur once per cycle.")
    savedPickles = true
    // Accumulate pickle data
    if (!pickleData.isEmpty && hooks.pickleJarPath.isPresent)
      accruedPickleData ++= pickleData
  }

  override def classesInOutputJar(): java.util.Set[String] = {
    outputJarContent.get().asJava
  }

}
