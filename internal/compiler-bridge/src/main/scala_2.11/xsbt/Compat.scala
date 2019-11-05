/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.PrintWriter

import xsbti.compile.Output

import scala.tools.nsc.Settings
import scala.util.{Properties, Try}
import java.nio.file
import java.nio.file.Path
import scala.reflect.io.VirtualFile

import scala.collection.mutable
import scala.tools.nsc.Global
import xsbti.AnalysisCallback.PickleData

import scala.tools.nsc.settings.MutableSettings
import scala.tools.nsc.util.ShowPickled

class PathBackedVirtualFile(p: Path) extends VirtualFile(p.toString, p.toString) {
  // Can't override file
}

abstract class Compat
object Compat {
  // IR is renamed to Results
  val Results = scala.tools.nsc.interpreter.IR

  // IMain in 2.13 accepts ReplReporter
  def replReporter(settings: Settings, writer: PrintWriter) = writer


  // Technically, we could do this before 2.12.8, but the return type of symData changed, so we'd
  // either have to invoke by reflection, or split out a third Compat.
  val versionOkForPickling = Properties.versionNumberString >= "2.12.8"

  // Again, we want to avoid a third Compat.  Note that at this YpickleJava is marked internalOnly,
  // so we can't extract it from regenerated string arguments.
  private lazy val pickleJavaMethod = Try(classOf[Settings].getMethod("YpickleJava")).toOption
  def pickleJava[G <: Global](global: G): Boolean =
    pickleJavaMethod.map(_.invoke(global.settings).asInstanceOf[MutableSettings#BooleanSetting].value).getOrElse(false)

  // Prepare pickle data for eventual storage, computing path within jar file from symbol ownership
  // and storing data in a class that does not rely on a shared scala library.
  // This is almost verbatim copied from scala.tools.nsc.PipelineMain, except that actually writing to the jar file
  // is deferred to AnalysisCallback, after the final incremental compilation cycle.
  def picklePaths[G <: Global](run: G#Run): Iterable[PickleData] =
    if (versionOkForPickling) {

      val rootPath = file.Paths.get("__ROOT__")
      val dirs = mutable.Map[G#Symbol,file.Path]()
      def packageDir(packSymbol: G#Symbol): file.Path = {
        if (packSymbol.isEmptyPackageClass) rootPath
        else if (dirs.contains(packSymbol)) dirs(packSymbol)
        else if (packSymbol.owner.isRoot) {
          val subDir = rootPath.resolve(packSymbol.encodedName)
          dirs.put(packSymbol, subDir)
          subDir
        } else {
          val base = packageDir(packSymbol.owner)
          val subDir = base.resolve(packSymbol.encodedName)
          dirs.put(packSymbol, subDir)
          subDir
        }
      }

      for ((s, p) <- run.symData) yield {
        val base = packageDir(s.owner)
        val path = base.resolve(s.encodedName + ".sig")
        //        val path = symToPath(s,true)
        val fqcn = s.fullNameString
        new PickleData(p, fqcn, p.bytes, p.writeIndex, path)
      }
    }
    else Iterable.empty
}


/** Defines compatibility utils for [[ZincCompiler]]. */
trait ZincGlobalCompat {
  protected def superDropRun(): Unit = ()
}

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler(settings: Settings, reporter: DelegatingReporter, output: Output): ZincCompiler =
    new ZincCompiler(settings, reporter, output)
}
