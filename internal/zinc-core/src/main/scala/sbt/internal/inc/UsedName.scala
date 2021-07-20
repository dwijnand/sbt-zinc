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

import java.{ util => ju }
import scala.{ collection => sc }

import xsbti.UseScope

case class UsedName private (nameHash: Int) extends xsbti.compile.UsedName {
  override def getNameHash: Int = nameHash
}

object UsedName {
  def apply(nameHash: Int): UsedName = make(nameHash)
  def make(nameHash: Int): UsedName = new UsedName(nameHash)
}

sealed abstract class UsedNames private {
  def isEmpty = true

  def ++(other: UsedNames): UsedNames
  def --(classes: Iterable[String]): UsedNames
  def iterator: Iterator[(String, sc.Map[UseScope, sc.Set[UsedName]])]

  def affectedNames(modifiedNames: ModifiedNames, from: String): String
}

object UsedNames {
  def fromJavaMap(map: ju.Map[String, Schema.UsedNameValues]): UsedNames = {
    new UsedNames {
      private def convert: UsedNames = {
        import scala.collection.JavaConverters._
        def fromUseScope(useScope: Schema.UseScope, id: Int): UseScope = useScope match {
          case Schema.UseScope.DEFAULT      => UseScope.Default
          case Schema.UseScope.IMPLICIT     => UseScope.Implicit
          case Schema.UseScope.PATMAT       => UseScope.PatMatTarget
          case Schema.UseScope.UNRECOGNIZED => ???
        }
        def fromUsedNamesMap(map: ju.Map[String, Schema.UsedNameValues]) = {
          for {
            (k, usedValues) <- map.asScala
          } yield k -> (for {
            used <- usedValues.getUsedNamesList.iterator.asScala
          } yield {
            val scope = fromUseScope(used.getScope, used.getScopeValue)
            scope -> used.getNameHashesList.asScala.iterator.map(UsedName(_)).toSet
          }).toMap
        }
        fromMultiMap(fromUsedNamesMap(map))
      }

      def ++(other: UsedNames) = convert ++ other
      def --(classes: Iterable[String]) = convert -- classes
      def iterator = convert.iterator
      def affectedNames(modifiedNames: ModifiedNames, from: String) = {
        val usedNameValues = map.get(from)
        val b = new StringBuilder()
        var first = true
        var i = 0
        val n = usedNameValues.getUsedNamesCount
        while (i < n) {
          val usedNames = usedNameValues.getUsedNames(i)
          val scope = usedNames.getScope
          var i2 = 0
          val n2 = usedNames.getNameHashesCount
          while (i2 < n2) {
            val nameHash = usedNames.getNameHashes(i2)
            if (modifiedNames.isModifiedRaw(scope, nameHash)) {
              if (first) first = false else b.append(", ")
              b.append(nameHash)
            }
            i2 += 1
          }
          i += 1
        }
        b.toString
      }
    }
  }

  def fromMultiMap(map: sc.Map[String, sc.Map[UseScope, sc.Set[UsedName]]]): UsedNames = {
    new UsedNames {
      def ++(other: UsedNames) = fromMultiMap(map ++ other.iterator)
      def --(classes: Iterable[String]) = fromMultiMap(map -- classes)
      def iterator = map.iterator
      def affectedNames(modifiedNames: ModifiedNames, from: String): String =
        map(from).iterator
          .flatMap { case (scope, names) => names.filter(modifiedNames.isModified(scope, _)) }
          .mkString(", ")
    }
  }
}
