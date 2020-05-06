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
import java.nio.file
import java.nio.file.Path

import xsbti.AnalysisCallback.PickleData
import xsbti.compile.Output

import scala.reflect.io.VirtualFile
import scala.tools.nsc.{ Global, Settings }

// Virtual file that can claim to have an underlying file.
class PathBackedVirtualFile(p: Path) extends VirtualFile(p.toString, p.toString) {
  override def file = p.toFile
}

abstract class Compat
object Compat {
  // IR is renamed to Results
  val Results = scala.tools.nsc.interpreter.IR

  // IMain in 2.13 accepts ReplReporter
  def replReporter(settings: Settings, writer: PrintWriter) = writer

  def pickleJava[G <: Global](global: G): Boolean = false

  // Prepare pickle data for eventual storage, computing path within jar file from symbol ownership
  // and storing data in a class that does not rely on a shared scala library.
  // This is almost verbatim copied from scala.tools.nsc.PipelineMain, except that actually writing to the jar file
  // is deferred to AnalysisCallback, after the final incremental compilation cycle.
  def picklePaths[G <: Global](run: G#Run): Iterable[PickleData] = Iterable.empty
}

/** Defines compatibility utils for [[ZincCompiler]]. */
trait ZincGlobalCompat {
  protected def superDropRun(): Unit = ()
}

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler(settings: Settings, reporter: DelegatingReporter, output: Output): ZincCompiler =
    new ZincCompiler(settings, reporter, output)
}
