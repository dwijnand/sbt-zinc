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

case class UsedName private (nameHash: Int) extends xsbti.compile.UsedName {
  override def getNameHash: Int = nameHash
}

object UsedName {
  def apply(nameHash: Int): UsedName = make(nameHash)
  def make(nameHash: Int): UsedName = new UsedName(nameHash)
}
