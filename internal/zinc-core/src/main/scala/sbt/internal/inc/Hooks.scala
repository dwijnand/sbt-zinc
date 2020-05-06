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
package sbt.internal.inc
import java.io.File
import java.nio.file.Path
import java.util.Optional
import java.util.function.{ BiConsumer, BiFunction }

import sbt.util.InterfaceUtil
import xsbti.api.AnalyzedClass
import xsbti.compile.{ CompileAnalysis, ExternalHooks, SourceSource }

object Hooks {
  private val SOURCE_SOURCE = "SOURCESOURCE"
  private val PROJECT_JAR_CHANGES = "PROJECTJARCHANGES"
  private val QUICK_API = "QUICKAPI"
  private val GET_PROVENANCE = "GETPROVENANCE"
  private val GET_JAREXEMPTION = "JAREXEMPTION"
  private val GET_INVALIDATION_PROFILER = "GETINVALIDATIONPROFILER"
  private val GET_ANALYSIS_PEEK = "GETANALYSISPEEK"
  private val GET_ADVANCE_PHASE = "GETADVANCEPHASE"

  private[sbt] def sourceSource(hooks: ExternalHooks): SourceSource = {
    val o = hooks.extraHooks().get(SOURCE_SOURCE)
    if (o eq null)
      SourceSource.empty
    else {
      val f = o.asInstanceOf[Path => String]
      new SourceSource {
        override def available() = true
        override def apply(path: Path) = f(path)
      }
    }
  }

  def addSourceSource(m: java.util.Map[String, Object], ss: Path => String): Unit = {
    m.put(SOURCE_SOURCE, ss)
  }

  private[sbt] def changedExternalJars(hooks: ExternalHooks): Int = {
    val o = hooks.extraHooks().get(PROJECT_JAR_CHANGES)
    if (o eq null)
      -1
    else
      o.asInstanceOf[Integer]
  }

  def addChangedExternalJars(m: java.util.Map[String, Object], count: Int): Unit = {
    m.put(PROJECT_JAR_CHANGES, new Integer(count))
  }

  /**
   * Convention:
   * None => Found class somewhere outside of project.  No analysis possible.
   * Some(analyzed) if analyzed.provenance.isEmpty => Couldn't find it.
   * Some(analyzed) => good
   */
  private[sbt] def quickAPI(hooks: ExternalHooks): (File, String) => Option[AnalyzedClass] = {
    val o = hooks.extraHooks().get(QUICK_API)
    if (o eq null)(_, _) => None
    else {
      val fj = o.asInstanceOf[(File, String) => Optional[AnalyzedClass]]
      (f, c) => InterfaceUtil.toOption[AnalyzedClass](fj(f, c))
    }
  }

  def addQuickAPI(
      m: java.util.Map[String, Object],
      f: (File, String) => Optional[AnalyzedClass]
  ): Unit = {
    m.put(QUICK_API, f)

  }

  private[sbt] def getProvenance(hooks: ExternalHooks): File => String = {
    val o = hooks.extraHooks().get(GET_PROVENANCE)
    if (o eq null)
      _ => ""
    else
      o.asInstanceOf[File => String]
  }

  def addGetProvenance(m: java.util.Map[String, Object], f: File => String): Unit = {
    m.put(GET_PROVENANCE, f)
  }

  private[sbt] def getIgnoreDiffInJar(hooks: ExternalHooks): String => Boolean = {
    val o = hooks.extraHooks().get(GET_JAREXEMPTION)
    if (o eq null)
      _ => false
    else o.asInstanceOf[String => Boolean]
  }

  def addIgnoreDiffsInJar(m: java.util.Map[String, Object], f: String => Boolean): Unit = {
    m.put(GET_JAREXEMPTION, f)
  }

  private[sbt] def getInvalidationProfiler(hooks: ExternalHooks): InvalidationProfiler = {
    val o = hooks.extraHooks().get(GET_INVALIDATION_PROFILER)
    Option(o).fold(InvalidationProfiler.empty)(_.asInstanceOf[InvalidationProfiler])
  }

  def addInvalidationProfiler(
      m: java.util.Map[String, Object],
      invalidationProfiler: InvalidationProfiler
  ): Unit = {
    m.put(GET_INVALIDATION_PROFILER, invalidationProfiler)
  }

  private[sbt] def getAnalysisPeek(hooks: ExternalHooks): (Int, Option[Analysis]) => Unit = {
    val o = hooks.extraHooks().get(GET_ANALYSIS_PEEK)
    Option(o) match {
      case Some(fj0: BiConsumer[_, _]) =>
        val fj = fj0.asInstanceOf[BiConsumer[Int, Optional[CompileAnalysis]]]
        (cycle: Int, analysis: Option[CompileAnalysis]) =>
          fj.accept(
            cycle,
            analysis.fold[Optional[CompileAnalysis]](Optional.empty())(Optional.of(_))
          )
      case None =>
        (_, _) => None
    }
  }

  def addAnalysisPeek(
      m: java.util.Map[String, Object],
      f: (Int, Option[Analysis]) => Unit
  ): Unit = {
    val fj = new BiConsumer[Int, Optional[CompileAnalysis]] {
      override def accept(cycle: Int, analysis: Optional[CompileAnalysis]): Unit =
        f(cycle, if (analysis.isPresent) Some(analysis.get.asInstanceOf[Analysis]) else None)
    }
    m.put(GET_ANALYSIS_PEEK, fj)
  }

  def addAdvancePhase(m: java.util.Map[String, Object], f: (String, String) => Unit): Unit = {
    val fj = new BiConsumer[String, String] {
      override def accept(prev: String, next: String) = f(prev, next)
    }
    m.put(GET_ADVANCE_PHASE, fj)
  }

  def getAdvancePhase(hooks: ExternalHooks): (String, String) => Unit = {
    val o = hooks.extraHooks().get(GET_ADVANCE_PHASE)
    Option(o) match {
      case Some(fj0: BiConsumer[_, _]) =>
        val fj = fj0.asInstanceOf[BiConsumer[String, String]]
        (prev: String, next: String) => fj.accept(prev, next)
      case None =>
        (_, _) => {}
    }

  }

}
