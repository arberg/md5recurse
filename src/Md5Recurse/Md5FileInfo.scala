package Md5Recurse

import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException
import java.nio.file.attribute.UserDefinedFileAttributeView

import scala.collection.{Map, immutable}
import scala.io.Source
//import scalax.file
//import scalax.file.{FileSystem, Path}
//
//// http://jesseeichar.github.io/scala-io-doc/0.4.3/index.html#!/file/string_to_file
//import scalax.file.defaultfs.DefaultPath
//import scalax.file.ImplicitConversions.jfile2path
//import scalax.file.ImplicitConversions.string2path
//import scalax.file.ImplicitConversions.defaultPath2jfile
//import scalax.file.defaultfs.DefaultPath
//import scalax.file.Path
//import scalax.file.ImplicitConverters._

/**
  * Created by Alex on 20-08-2016.
  */
object Md5FileInfo {

  val ATTR_MD5RECURSE = "md5recurse" // java or filesystem prefixes 'user.'

  // read file and generate md5sum
  def readFileGenerateMd5Sum(file: File, updateFileAttribute: Boolean): Option[Md5FileInfo] = {
    val md5 = Md5Recurse.md5Sum(file.getAbsolutePath)
    if (md5.isDefined) {
      val md5FileInfo: Md5FileInfo = create(file, md5.get.md5, md5.get.isBinary)
      val md5FileInfoUpdated = if (updateFileAttribute) Md5FileInfo.updateMd5FileAttribute(file, md5FileInfo) else md5FileInfo
      Some(md5FileInfoUpdated)
    }
    else None
  }

  //  def createAndGenerate(fileInfo: FileInfo) = {
  //    new Md5FileInfo(fileInfo, Md5Tool.md5Sum(fileInfo.file().getAbsolutePath))
  //  }
  //  def createWithMd5(fileInfo: FileInfo, md5: String, isBinary: Boolean) = {
  //    new Md5FileInfo(fileInfo, new Md5SumInfo("", md5, isBinary, fileInfo.file().getPath()))
  //  }
  //  def createAndGenerate(file: File): Md5FileInfo = {
  //    createAndGenerate(FileInfo.create(file))
  //  }
  def create(file: File, md5: String, isBinary: Boolean) = {
    new Md5FileInfo(FileInfoBasic.create(file), md5, isBinary)
  }

  def parseMd5DataLine(dir: String, dataLine: String) = {
    // lastMod will be negative if file dated before 1970.
    val timestampRegex =
      """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)\s(.*)""".r
    val (md5, lastMod, size, isBinary, f) = dataLine match {
      case timestampRegex(md5, b, lastMod, size, f) =>
        (md5, lastMod, size, b == "1", f)
      case _ => throw new RuntimeException(dataLine + "\n line did not match timestamp-expression")
    }
    new Md5FileInfo(new FileInfoBasic(lastMod.toLong, size.toLong, dir, f), md5, isBinary)
  }

  @throws(classOf[ParseException])
  def parseMd5FileAttribute(file: File, dataLine: String) = {
    // lastMod will be negative if file dated before 1970.
    val timestampRegex =
      """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)""".r
    val (md5, lastMod, size, isBinary) = dataLine match {
      case timestampRegex(md5, b, lastMod, size) =>
        (md5, lastMod, size, b == "1")
      case _ => {
        throw new ParseException(file.getAbsolutePath + ": File attribute content corrupt: '" + dataLine + " ' ")
      }
    }
    new Md5FileInfo(FileInfoBasic.create(lastMod.toLong, size.toLong, file), md5, isBinary)
  }

  private def doLog: Boolean = {
    Config.debugLog || Config.it != null && Config.it.verbose >= 3 // Config null when directly executed from unit test
  }

  @throws(classOf[ParseException])
  def readMd5FileAttribute(file: File): Option[Md5FileInfo] = {
    val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
    val dataLine: Option[String] = FileUtil.getAttr(attrView, ATTR_MD5RECURSE)
    if (dataLine.isDefined) {
      if (doLog) println(file + ": Attr read: '" + dataLine.get + "'")
      Some(parseMd5FileAttribute(file, dataLine.get))
    } else {
      if (doLog) println(file + ": No Attr")
      None
    }
  }

  def readDirFile(md5dataFilename: String): Map[String, Md5FileInfo] = {
    val md5dataFile = new File(md5dataFilename)
    val fileSet = readFile(md5dataFile)
    val o = fileSet.getDir(md5dataFile.getParentFile)
    if (o.isEmpty) new immutable.HashMap[String, Md5FileInfo]() else o.get
  }

  private def getFileDir(file: File): String = {
    file.getParentFile.getAbsolutePath
  }

  def readFile(md5dataFile: File): DirAndFileMap[Md5FileInfo] = {
    val fileSet = new DirAndFileMap[Md5FileInfo];
    val encoding = "UTF-8"
    var lineNo = 1;
    var lastLine = "";
    try {
      var prefixDir: String = ""
      var currentDir: String = getFileDir(md5dataFile)
      var dirMap = fileSet.getOrCreateDir(new File(currentDir))
      val source = Source.fromFile(md5dataFile, encoding)
      for (line <- source.getLines) {
        lastLine = line
        if (line.startsWith("+")) {
          prefixDir = line.substring(1)
        } else if (line.startsWith(">")) {
          fileSet.setDir(currentDir, dirMap)
          currentDir = new File(prefixDir + line.substring(1)).getAbsolutePath
          dirMap = fileSet.getOrCreateDir(new File(currentDir))
        } else {
          try {
            val fileInfo = Md5FileInfo.parseMd5DataLine(currentDir, line)
            dirMap += (fileInfo.fileName() -> fileInfo)
          } catch {
            case e: RuntimeException => System.err.println("Error occurred reading file " + md5dataFile + " @ line " + lineNo + " Parse error: " + lastLine)
          }
        }
        lineNo += 1
      }
      fileSet.setDir(currentDir, dirMap)
      source.close
    } catch {
      case e: FileNotFoundException =>
      case e: MalformedInputException => System.err.println("File in different charset encoding, read file with " + encoding + ": " + md5dataFile)
      case e: RuntimeException => System.err.println("Error occurred reading file " + md5dataFile + " @ line " + lineNo + "(will skip rest of file, and regenerate new): " + lastLine)
    }
    fileSet.setDoneBuilding;
    fileSet
  }

  def updateMd5FileAttribute(file: File, md5FileInfo: Md5FileInfo): Md5FileInfo = {
    val MAX_WRITE_ATTEMPTS = 5
    val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
    val oldAttr = FileUtil.getAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE)

    val allowUpdateFileModified = md5FileInfo.lastModified() == file.lastModified()

    // only update changes
    def doUpdate(inputMd5FileInfo: Md5FileInfo, i: Int): Md5FileInfo = {
      val newDataLine = inputMd5FileInfo.exportAttrLine
      if (!oldAttr.isDefined || oldAttr.get != newDataLine) {
        if (FileUtil.setAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE, newDataLine)) {
          if (Md5FileInfo.doLog) println(file + ": Attr written: '" + newDataLine + "'" + " - New lastModified " + file.lastModified())
        }
      }
      val updated = inputMd5FileInfo.createUpdateTimestamp
      if (updated.lastModified() != inputMd5FileInfo.lastModified() && i < MAX_WRITE_ATTEMPTS && allowUpdateFileModified)
        doUpdate(updated, i + 1)
      else
        inputMd5FileInfo
    }

    // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
    // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
    FileUtil.doWithLockedFile(file) {
      () => {
        val updatedMd5FileInfo = doUpdate(md5FileInfo, 0)
        if (file.lastModified() != updatedMd5FileInfo.lastModified() && allowUpdateFileModified) {
          println("WARNING: Failed to set fileAttribute with same lastModified as file timestamp")
          updatedMd5FileInfo.createUpdateTimestamp
        } else {
          updatedMd5FileInfo
        }
      }
    }
  }
}

class Md5FileInfo(fileInfo: FileInfoBasic, md5: String, isBinaryMd5: Boolean) {

  override def toString = exportDataLineFullPath

  def md5String = md5

  def getFileInfo(): FileInfoBasic = fileInfo

  def size(): Long = fileInfo.getSize

  def lastModified(): Long = fileInfo.getLastModified

  //  def file(): File = fileInfo.file
  def filePath(): String = fileInfo.getPath

  def getDirectoryPath(): String = fileInfo.getDirectoryPath

  def fileName(): String = fileInfo.getName

  def exportAttrLine = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineWithoutFile

  def exportDataLineFileName = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineFileName

  def exportDataLineFullPath = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineFullPath

  def exportMd5Line = md5 + " " + (if (isBinaryMd5) "*" else " ") + fileInfo.getName

  def createUpdateTimestamp = new Md5FileInfo(fileInfo.createUpdateLastModified(), md5, isBinaryMd5)
}