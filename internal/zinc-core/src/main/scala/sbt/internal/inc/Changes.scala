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

import scala.collection.JavaConverters._

import xsbti.{ UseScope, VirtualFileRef }
import xsbti.api.NameHash
import xsbti.compile.Changes
import xsbti.compile.{ APIChange => XAPIChange }
import xsbti.compile.{ InitialChanges => XInitialChanges }

final case class InitialChanges(
    internalSrc: Changes[VirtualFileRef],
    removedProducts: Set[VirtualFileRef],
    libraryDeps: Set[VirtualFileRef],
    external: APIChanges
) extends XInitialChanges {

  def isEmpty: Boolean =
    internalSrc.isEmpty &&
      removedProducts.isEmpty &&
      libraryDeps.isEmpty &&
      external.apiChanges.isEmpty

  def getInternalSrc: Changes[VirtualFileRef] = internalSrc
  def getRemovedProducts: java.util.Set[VirtualFileRef] = removedProducts.asJava
  def getLibraryDeps: java.util.Set[VirtualFileRef] = libraryDeps.asJava
  def getExternal: Array[XAPIChange] = external.apiChanges.toArray
}

final class APIChanges(val apiChanges: Iterable[APIChange]) {
  override def toString = "API Changes: " + apiChanges
  def allModified: Iterable[String] = apiChanges.map(_.modifiedClass)
}

sealed abstract class APIChange(val modifiedClass: String) extends XAPIChange {
  override def getModifiedClass: String = modifiedClass
}

/**
 * If we recompile a source file that contains a macro definition then we always assume that it's
 * api has changed. The reason is that there's no way to determine if changes to macros implementation
 * are affecting its users or not. Therefore we err on the side of caution.
 */
final case class APIChangeDueToMacroDefinition(modified0: String) extends APIChange(modified0)

/**
 * An APIChange that carries information about modified names.
 *
 * This class is used only when name hashing algorithm is enabled.
 */
final case class NamesChange(modified0: String, modifiedNames: ModifiedNames)
    extends APIChange(modified0)

final case class TraitPrivateMembersModified(modified: String) extends APIChange(modified)

/**
 * ModifiedNames are determined by comparing name hashes in two versions of an API representation.
 *
 * Note that we distinguish between sets of regular (non-implicit) and implicit modified names.
 * This distinction is needed because the name hashing algorithm makes different decisions based
 * on whether modified name is implicit or not. Implicit names are much more difficult to handle
 * due to difficulty of reasoning about the implicit scope.
 */
final case class ModifiedNames(names: Map[UseScope, Set[UsedName]]) {
  def in(scope: UseScope): Set[UsedName] = names.getOrElse(scope, Set.empty)

  def isModified(scope: UseScope, usedName: UsedName): Boolean = in(scope).contains(usedName)
  def isModifiedRaw(scope: UseScope, nameHash: Int): Boolean =
    in(scope).exists(_.nameHash == nameHash)

  override def toString = s"ModifiedNames(changes = ${names.mkString(", ")})"
}

object ModifiedNames {
  def compareTwoNameHashes(a: Array[NameHash], b: Array[NameHash]): ModifiedNames = {
    val xs = a.toSet
    val ys = b.toSet
    val changed = xs.union(ys).diff(xs.intersect(ys))
    val modifiedNames = changed.groupBy(_.scope).map {
      case (scope, nameHashes) =>
        scope -> nameHashes.map(name => UsedName(name.hash))
    }
    ModifiedNames(modifiedNames)
  }
}

abstract class UnderlyingChanges[A] extends Changes[A] {
  def added: Set[A]
  def removed: Set[A]
  def changed: Set[A]
  def unmodified: Set[A]

  import scala.collection.JavaConverters.setAsJavaSetConverter
  override def getAdded: java.util.Set[A] = added.asJava
  override def getChanged: java.util.Set[A] = changed.asJava
  override def getRemoved: java.util.Set[A] = removed.asJava
  override def getUnmodified: java.util.Set[A] = unmodified.asJava
  override def isEmpty = added.isEmpty && removed.isEmpty && changed.isEmpty

  override def toString: String = {
    s"""Changes(added = $added, removed = $removed, changed = $changed, unmodified = ...)""".stripMargin
  }
}
