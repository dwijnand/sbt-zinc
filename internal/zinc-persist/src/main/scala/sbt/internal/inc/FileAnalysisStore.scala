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

import java.io._
import java.util.Optional
import java.util.zip.{ ZipEntry, ZipInputStream }

import sbt.internal.shaded.com.google.protobuf.{ CodedInputStream, CodedOutputStream }
import sbt.internal.inc.binary.BinaryAnalysisFormat
import sbt.io.{ IO, Using }
import xsbti.compile.analysis.ReadWriteMappers
import xsbti.compile.{ AnalysisContents, AnalysisStore => XAnalysisStore }

import scala.util.control.Exception.allCatch

object FileAnalysisStore {
  private final val BinExtension = "bin"
  private final val analysisFileName = s"inc_compile.$BinExtension"
  private final val companionsFileName = s"api_companions.$BinExtension"

  def binary(analysisFile: File): XAnalysisStore =
    new BinaryFileStore(analysisFile, ReadWriteMappers.getEmptyMappers())
  def binary(analysisFile: File, mappers: ReadWriteMappers): XAnalysisStore =
    new BinaryFileStore(analysisFile, mappers)

  private final class BinaryFileStore(file: File, readWriteMappers: ReadWriteMappers)
      extends XAnalysisStore {

    private final val format = new BinaryAnalysisFormat(readWriteMappers)
    private final val TmpEnding = ".tmp"

    /**
     * Get `CompileAnalysis` and `MiniSetup` instances for current `Analysis`.
     */
    override def get: Optional[AnalysisContents] = {
      import JavaInterfaceUtil.EnrichOption
      val nestedRead: Option[Option[AnalysisContents]] = allCatch.opt {
        Using.zipInputStream(new FileInputStream(file)) { inputStream =>
          lookupEntry(inputStream, analysisFileName)
          val reader = CodedInputStream.newInstance(inputStream)
          val (analysis, miniSetup) = format.read(reader)
          val analysisWithAPIs = allCatch.opt {
            lookupEntry(inputStream, companionsFileName)
            format.readAPIs(reader, analysis, miniSetup.storeApis)
          }

          analysisWithAPIs.map(analysis => AnalysisContents.create(analysis, miniSetup))
        }
      }
      nestedRead.flatten.toOptional
    }

    override def unsafeGet: AnalysisContents = get.get

    /**
     * Write the zipped analysis contents into a temporary file before
     * overwriting the old analysis file and avoiding data race conditions.
     *
     * See https://github.com/sbt/zinc/issues/220 for more details.
     */
    override def set(contents: AnalysisContents): Unit = {
      val analysis = contents.getAnalysis
      val setup = contents.getMiniSetup
      val tmpAnalysisFile = File.createTempFile(file.getName, TmpEnding)
      if (!file.getParentFile.exists())
        file.getParentFile.mkdirs()

      val outputStream = new FileOutputStream(tmpAnalysisFile)
      Using.zipOutputStream(outputStream) { outputStream =>
        val protobufWriter = CodedOutputStream.newInstance(outputStream)
        outputStream.putNextEntry(new ZipEntry(analysisFileName))
        format.write(protobufWriter, analysis, setup)
        outputStream.closeEntry()

        outputStream.putNextEntry(new ZipEntry(companionsFileName))
        format.writeAPIs(protobufWriter, analysis, setup.storeApis())
        outputStream.closeEntry()
      }
      IO.move(tmpAnalysisFile, file)
    }
  }

  private def lookupEntry(in: ZipInputStream, name: String): Unit =
    Option(in.getNextEntry) match {
      case Some(entry) if entry.getName == name => ()
      case Some(_)                              => lookupEntry(in, name)
      case None                                 => sys.error(s"$name not found in the zip file")
    }
}
