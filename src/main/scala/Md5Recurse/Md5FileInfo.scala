package Md5Recurse

import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException
import java.nio.file.attribute.UserDefinedFileAttributeView

import scala.collection.Map
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
    val defaultAnswer: Option[Md5FileInfo] = None
    FileUtil.doWithLockedFile(file, defaultAnswer) {
      () => {
        ExecutionLog.current.readFileAndGeneratedMd5 = true
        val lastModified = file.lastModified() // Read last modified before scanning file, in case file changes during scan, we could also lock file, but that seems unnecessary and would cause some reads to fail
        val md5 = Md5Recurse.md5Sum(file.getPath)
        if (md5.isDefined) {
          val md5FileInfo: Md5FileInfo = create(file, lastModified, md5.get.md5, md5.get.isBinary)
          val md5FileInfoUpdated = if (updateFileAttribute) Md5FileInfo.updateMd5FileAttribute(file, md5FileInfo, true) else md5FileInfo
          Some(md5FileInfoUpdated)
        }
        else None
      }
    }
  }

  def create(file: File, lastModified: Long, md5: String, isBinary: Boolean) = {
    new Md5FileInfo(FileInfoBasic.create(lastModified, file), md5, isBinary)
  }

  // Keep regexp as field, because of performance though it uses 20-30% more memory but takes 30% less time
  val md5DataLineRegExp =
    """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)\s(.*)""".r

  def parseMd5DataLine(dir: String, dataLine: String) = {
    // lastMod will be negative if file dated before 1970.
    val (md5, lastMod: String, size, isBinary, filename) = dataLine match {
      case md5DataLineRegExp(md5, b, lastMod, size, f) => (md5, lastMod, size, b == "1", f)
      case _ => throw new RuntimeException(dataLine + "\n line did not match timestamp-expression")
    }
    new Md5FileInfo(new FileInfoBasic(lastMod.toLong, size.toLong, dir, filename), md5, isBinary)
  }

  val md5DataLineFileAttributeRegExp = """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)""".r

  @throws(classOf[ParseException])
  def parseMd5FileAttribute(file: File, dataLine: String) = {
    // lastMod will be negative if file dated before 1970.
    val (md5, lastMod, size, isBinary) = dataLine match {
      case md5DataLineFileAttributeRegExp(md5, b, lastMod, size) => (md5, lastMod, size, b == "1")
      case _ => {
        throw new ParseException(file.getCanonicalPath + ": File attribute content corrupt: '" + dataLine + " ' ")
      }
    }
    new Md5FileInfo(FileInfoBasic.create(lastMod.toLong, size.toLong, file), md5, isBinary)
  }

  private def doLog: Boolean = {
    Config.debugLog || Config.it != null && Config.it.logMd5ScansSkippedAndPrintDetails // Config null when directly executed from unit test
  }

  @throws(classOf[ParseException])
  def readMd5FileAttribute(file: File): Option[Md5FileInfo] = {
    val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
    val dataLine: Option[String] = FileUtil.getAttr(attrView, ATTR_MD5RECURSE)
    if (Config.it.logMd5ScansSkippedAndLocalAndAttributeReads) println("Reading fileAttribute " + file.getName + ": " + (if (dataLine.isDefined) "Found" else "Not available"))
    if (dataLine.isDefined) {
      if (doLog) println(file + ": Attr read: '" + dataLine.get + "'")
      Some(parseMd5FileAttribute(file, dataLine.get))
    } else {
      if (doLog) println(file + ": No Attr")
      None
    }
  }

  def readDirFile(md5dataFilename: String): Option[Map[String, Md5FileInfo]] = {
    val md5dataFile = new File(md5dataFilename)
    val fileSet = readMd5DataFile(md5dataFile)
    fileSet.getDir(md5dataFile.getParentFile)
  }

  private def getFileDir(file: File): String = {
    file.getParentFile.getCanonicalPath
  }

  /**
    * Reads MD5 data files. The reader supports reading concatenated files, where the same directory is mentioned several times. If files appear multiple times, the last file will be remembered
    *
    * @param md5dataFile
    * @return
    */
  def readMd5DataFile(md5dataFile: File): DirToFileMap[Md5FileInfo] = {
    val dirToFileMap = new DirToFileMap[Md5FileInfo];
    val encoding = "UTF-8"
    var lineNo = 1;
    var lastLine = "";
    try {
      var prefixDir: String = ""
      var currentDir: String = getFileDir(md5dataFile)
      var fileMap = dirToFileMap.getOrCreateDir(new File(currentDir))
      val source = Source.fromFile(md5dataFile, encoding)
      for (line <- source.getLines) {
        lastLine = line
        if (line.startsWith("+")) {
          // Unused: I don't think I ever write this, to be removed
          prefixDir = line.substring(1)
        } else if (line.startsWith(">")) {
          // New directory
          dirToFileMap.setDir(currentDir, fileMap)
          currentDir = prefixDir + line.substring(1)
          fileMap = dirToFileMap.getOrCreateDir(new File(currentDir))
        } else {
          // New file
          try {
            val fileInfo = Md5FileInfo.parseMd5DataLine(currentDir, line)
            fileMap += (fileInfo.fileName() -> fileInfo)
          } catch {
            case e: RuntimeException => System.err.println("Error occurred reading file " + md5dataFile + " @ line " + lineNo + " Parse error: " + lastLine)
          }
        }
        lineNo += 1
      }
      dirToFileMap.setDir(currentDir, fileMap)
      source.close
    } catch {
      case e: FileNotFoundException =>
      case e: MalformedInputException => System.err.println("File in different charset encoding, read file with " + encoding + ": " + md5dataFile)
      case e: RuntimeException => System.err.println("Error occurred reading file " + md5dataFile + " @ line " + lineNo + "(will skip rest of file, and regenerate new): " + lastLine)
    }
    dirToFileMap.setDoneBuilding;
    dirToFileMap
  }


  def updateMd5FileAttribute(file: File, md5FileInfo: Md5FileInfo): Md5FileInfo = {
    updateMd5FileAttribute(file, md5FileInfo, false)
  }

  def updateMd5FileAttribute(file: File, md5FileInfo: Md5FileInfo, isFileAlreadyLocked: Boolean): Md5FileInfo = {
    val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
    val oldAttr = FileUtil.getAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE)

    // only update changes
    def doUpdate(inputMd5FileInfo: Md5FileInfo) {
      val newDataLine = inputMd5FileInfo.exportAttrLine
      if (!oldAttr.isDefined || oldAttr.get != newDataLine) {
        if (FileUtil.setAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE, newDataLine)) {
          if (Md5FileInfo.doLog) println(file + ": Attr written: '" + newDataLine + "'" + " - New lastModified " + file.lastModified())
        }
      }
    }

    // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
    // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
    val updateAttributeFunction = () => {
      val lastModifiedBeforeAttributeUpdateWasEqual = md5FileInfo.lastModified() == file.lastModified() // todo
      doUpdate(md5FileInfo)
      if (lastModifiedBeforeAttributeUpdateWasEqual && file.lastModified() != md5FileInfo.lastModified()) {
        file.setLastModified(md5FileInfo.lastModified())
        if (file.lastModified() != md5FileInfo.lastModified())
          println("WARNING: Failed to set fileAttribute with same lastModified as file timestamp")
      }
    }
    if (isFileAlreadyLocked) updateAttributeFunction.apply() else FileUtil.doWithLockedFile(file)(updateAttributeFunction)
    md5FileInfo
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