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

package xsbt

import java.io.PrintWriter
import xsbti.compile.Output
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.ReplReporterImpl
import scala.collection.mutable
import xsbti.AnalysisCallback.PickleData
import scala.tools.nsc.Global
import java.nio.file
import scala.util.Try
import java.nio.file.Path
import scala.reflect.io.VirtualFile

// Virtual file that can claim to have an underlying file.
class PathBackedVirtualFile(p: Path) extends VirtualFile(p.toString, p.toString) {
  override def file = p.toFile
}

abstract class Compat
object Compat {
  // IR is renanmed to Results
  val Results = scala.tools.nsc.interpreter.Results

  // IMain in 2.13 accepts ReplReporter
  def replReporter(settings: Settings, writer: PrintWriter) =
    new ReplReporterImpl(settings, writer)

  def pickleJava[G <: Global](global: G): Boolean = false // global.settings.YpickleJava

  // Extract pickle data and package without using any scalac classes.
  def picklePaths[G <: Global](run: G#Run): Iterable[PickleData] = {

    val rootPath = file.Paths.get("__ROOT__")
    val dirs = mutable.Map[G#Symbol, file.Path]()

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
}

/** Defines compatibility utils for [[ZincCompiler]]. */
trait ZincGlobalCompat {
  protected def superDropRun(): Unit = ()
}

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler(settings: Settings, reporter: DelegatingReporter, output: Output): ZincCompiler =
    new ZincCompiler(settings, reporter, output)
}
