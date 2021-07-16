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

import xsbti.compile.{ UsedName => XUsedName }
import xsbti.UseScope

case class UsedName private (nameHash: Int, scopes: java.util.EnumSet[UseScope]) extends XUsedName {
  override def getNameHash: Int = nameHash
  override def getScopes: java.util.EnumSet[UseScope] = scopes
}

object UsedName {
  def apply(nameHash: Int, scopes: Iterable[UseScope] = Nil): UsedName = {
    val useScopes = java.util.EnumSet.noneOf(classOf[UseScope])
    scopes.foreach(useScopes.add)
    make(nameHash, useScopes)
  }

  def make(nameHash: Int, useScopes: java.util.EnumSet[UseScope]): UsedName = {
    new UsedName(nameHash, useScopes)
  }
}
