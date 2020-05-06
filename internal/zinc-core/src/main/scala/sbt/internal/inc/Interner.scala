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

import scala.collection.mutable

/**
 * Used to faciliate sharing of identical objects within large structures, such as analyses or AnalysisCallback.
 * This is less ambitious and lighter-weight than String#intern, which operates globally and across threads.
 */
class Interner[A] {
  private val interned = new mutable.HashMap[A, A]
  private var nReused = 0
  def status = (interned.size, nReused)
  def apply(s: A): A = {
    nReused += 1
    interned.getOrElseUpdate(s, s)
  }
}
