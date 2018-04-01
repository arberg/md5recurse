package Md5Recurse

import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException
import java.nio.file.attribute.UserDefinedFileAttributeView

import scalax.file.Path

import scala.collection.Map
import scala.util.matching.Regex
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
                    val md5FileInfoUpdated = if (updateFileAttribute) Md5FileInfo.updateMd5FileAttribute(file, md5FileInfo, isFileAlreadyLocked = true) else md5FileInfo
                    Some(md5FileInfoUpdated)
                }
                else {
                    None
                }
            }
        }
    }

    def create(file: File, lastModified: Long, md5: String, isBinary: Boolean): Md5FileInfo = {
        new Md5FileInfo(FileInfoBasic.create(lastModified, file), md5, isBinary)
    }

    // Keep regexp as field, because of performance though it uses 20-30% more memory but takes 30% less time
    val md5SumCommentDataLineRegExp: Regex =
        """#>([-]?\d+);(\d+)""".r
    val md5SumHashDataLineRegExp: Regex = """([0-9a-f]+)\s([*]?)(.*)""".r
    val md5DataLineRegExp: Regex = """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)\s(.*)""".r

    def parseMd5DataLine(dir: String, dataLine: String): Md5FileInfo = {
        // lastMod will be negative if file dated before 1970.
        val (md5, lastMod: String, size, isBinary, filename) = dataLine match {
            case md5DataLineRegExp(md5, b, lastMod, size, f) => (md5, lastMod, size, b == "1", f)
            case _ => throw new RuntimeException(dataLine + "\n line did not match timestamp-expression")
        }
        new Md5FileInfo(new FileInfoBasic(lastMod.toLong, size.toLong, dir, filename), md5, isBinary)
    }

    def parseMd5SumDataLine(dir: String, commentLine: String, dataLine: String): Md5FileInfo = {
        // lastMod will be negative if file dated before 1970.
        val (lastMod: String, size) = commentLine match {
            case md5SumCommentDataLineRegExp(lastMod, size) => (lastMod, size)
            case _ => throw new RuntimeException(commentLine + "\n comment line did not match timestamp-expression")
        }
        val (md5, isBinary, filename) = dataLine match {
            case md5SumHashDataLineRegExp(md5, bStar, f) => (md5, bStar == "*", f)
            case _ => throw new RuntimeException(dataLine + "\n line did not match timestamp-expression")
        }
        new Md5FileInfo(new FileInfoBasic(lastMod.toLong, size.toLong, dir, filename), md5, isBinary)
    }

    val md5DataLineFileAttributeRegExp: Regex = """([0-9a-f]+)\s([01])\s([-]?\d+)\s(\d+)""".r

    @throws(classOf[ParseException])
    def parseMd5FileAttribute(file: File, dataLine: String): Md5FileInfo = {
        // lastMod will be negative if file dated before 1970.
        val (md5, lastMod, size, isBinary) = dataLine match {
            case md5DataLineFileAttributeRegExp(md5, b, lastMod, size) => (md5, lastMod, size, b == "1")
            case _ =>
                throw new ParseException(file.getCanonicalPath + ": File attribute content corrupt: '" + dataLine + " ' ")
        }
        new Md5FileInfo(FileInfoBasic.create(lastMod.toLong, size.toLong, file), md5, isBinary)
    }

    private def logEnabled: Boolean = {
        Config.debugLog || Config.it != null && Config.it.logMd5ScansSkippedAndPrintDetails // Config null when directly executed from unit test
    }

    @throws(classOf[ParseException])
    def readMd5FileAttribute(file: File): Option[Md5FileInfo] = {
        val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
        val dataLine: Option[String] = FileUtil.getAttr(attrView, ATTR_MD5RECURSE)
        if (Config.it.logMd5ScansSkippedAndLocalAndAttributeReads) println("Reading fileAttribute " + file.getName + ": " + (if (dataLine.isDefined) "Found" else "Not available"))
        if (dataLine.isDefined) {
            if (logEnabled) println(file + ": Attr read: '" + dataLine.get + "'")
            Some(parseMd5FileAttribute(file, dataLine.get))
        } else {
            if (logEnabled) println(file + ": No Attr")
            None
        }
    }

    def readDirFile(md5dataFilename: String, md5sumWithDataInComment: Boolean): Option[Map[String, Md5FileInfo]] = {
        if (Config.it.logMd5ScansSkippedAndLocalAndAttributeReads) println("Reading local md5data: " + md5dataFilename)
        val md5dataFile = new File(md5dataFilename)
        val fileSet = readMd5DataFile(md5dataFile, md5sumWithDataInComment)
        fileSet.getDir(md5dataFile.getParentFile)
    }

    /**
      * Reads MD5 data files. The reader supports reading concatenated files, where the same directory is mentioned several times. If files appear multiple times, the last file will be remembered
      *
      * @param md5dataFile the data file
      * @param md5sumWithDataInComment iff true then Reads *.md5 files with comments with timestamp and file size
      * @return
      */
    def readMd5DataFile(md5dataFile: File, md5sumWithDataInComment: Boolean): DirToFileMap = {
        val dirToFileMap = new DirToFileMap
        val encoding = "UTF-8"
        var lineNo = 1
        var lastLine = ""
        var lastCommentForLine: Option[String] = None
        var currentDir: String = md5dataFile.getParent
        var fileList = dirToFileMap.getOrCreateDir(currentDir) // Initial fileList is used for local md5data files
        var warnForMissingMd5DataComment = false
        try {
            for (line <- Path(md5dataFile).lines()) {
                lastLine = line
                if (md5sumWithDataInComment) {
                    // For md5sum files we parse an fill in the map immediately
                    if (line.startsWith("#")) {
                        lastCommentForLine = Some(line)
                    } else if (lastCommentForLine.isDefined) {
                        try {
                            fileList.addToMap(Md5FileInfo.parseMd5SumDataLine(currentDir, lastCommentForLine.get, line))
                        } catch {
                            case _: RuntimeException => Console.err.println("Error occurred parsing md5data line: " + line)
                        }
                        lastCommentForLine = None
                    } else if (line.nonEmpty) {
                        if (!warnForMissingMd5DataComment) {
                            Console.err.println("Missing md5data comment in File " + md5dataFile)
                            warnForMissingMd5DataComment = true
                        }
                    }
                } else {
                    // For md5data files we postpone parsing and esp filling in map with Md5FileInfo to avoid the memory usage blowup for the entire global file
                    if (line.startsWith(">")) {
                        // New directory
                        currentDir = line.substring(1)
                        // Converting to absolute dir has a tiny performance price. It makes scanning an 82MB global file with 122.000 dirs take 1.6 sec instead of 0.7 sec.
                        // It makes possible for a user to modify global files without to much care about getting slashes and absolute paths correct in string+case comparison perfect
                        // This absolute conversion has only been done on the directory not the files in the directory
                        val convertedCurrentDir = Path.fromString(currentDir).toAbsolute.path
                        fileList = dirToFileMap.getOrCreateDir(convertedCurrentDir)
                    } else {
                        // New file
                        fileList.addToList(line)
                    }
                }
                lineNo += 1
            }
        } catch {
            case _: FileNotFoundException =>
            case _: MalformedInputException => Console.err.println("File in different charset encoding, read file with " + encoding + ": " + md5dataFile)
            case _: RuntimeException => Console.err.println("Error occurred reading file " + md5dataFile + " @ line " + lineNo + "(will skip rest of file, and regenerate new): " + lastLine)
        }
        dirToFileMap.setDoneBuilding()
        dirToFileMap
    }

    def updateMd5FileAttribute(file: File, md5FileInfo: Md5FileInfo): Md5FileInfo = {
        updateMd5FileAttribute(file, md5FileInfo, isFileAlreadyLocked = false)
    }

    def updateMd5FileAttribute(file: File, md5FileInfo: Md5FileInfo, isFileAlreadyLocked: Boolean): Md5FileInfo = {
        val attrView: UserDefinedFileAttributeView = FileUtil.attrView(file)
        val oldAttr = FileUtil.getAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE)

        // only update changes
        def doUpdate(inputMd5FileInfo: Md5FileInfo) {
            val newDataLine = inputMd5FileInfo.exportAttrLine
            if (oldAttr.isEmpty || oldAttr.get != newDataLine) {
                if (FileUtil.setAttr(attrView, Md5FileInfo.ATTR_MD5RECURSE, newDataLine)) {
                    if (Md5FileInfo.logEnabled) println(file + ": Attr written: '" + newDataLine + "'" + " - New lastModified " + file.lastModified())
                }
            }
        }

        // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
        // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
        val updateAttributeFunction = () => {
            val lastModifiedBeforeAttributeUpdateWasEqual = md5FileInfo.lastModified == file.lastModified()
            doUpdate(md5FileInfo)
            if (lastModifiedBeforeAttributeUpdateWasEqual && file.lastModified != md5FileInfo.lastModified) {
                file.setLastModified(md5FileInfo.lastModified)
                if (file.lastModified() != md5FileInfo.lastModified) {
                    println("WARNING: Failed to set fileAttribute with same lastModified as file timestamp")
                }
            }
        }
        if (isFileAlreadyLocked) updateAttributeFunction.apply() else FileUtil.doWithLockedFile(file)(updateAttributeFunction)
        md5FileInfo
    }
}

class Md5FileInfo(fileInfo: FileInfoBasic, md5: String, isBinaryMd5: Boolean) {

    override def toString: String = exportDataLineFullPath

    def md5String: String = md5

    def getFileInfo: FileInfoBasic = fileInfo

    def size: Long = fileInfo.getSize()

    def lastModified: Long = fileInfo.getLastModified()

    //  def file(): File = fileInfo.file
    def filePath: String = fileInfo.getPath()

    def getDirectoryPath: String = fileInfo.getDirectoryPath()

    def fileName: String = fileInfo.getName

    def exportAttrLine: String = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineWithoutFile

    def exportDataLineFileName: String = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineFileName

    def exportDataLineFullPath: String = md5 + " " + (if (isBinaryMd5) "1" else "0") + " " + fileInfo.exportLineFullPath

    def exportDataLineComment: String = s"#>" + lastModified + ";" + size

    def exportMd5Line: String = exportDataLineComment + "\n" + md5 + " " + (if (isBinaryMd5) "*" else " ") + fileInfo.getName

    def createUpdateTimestamp = new Md5FileInfo(fileInfo.createUpdateLastModified(), md5, isBinaryMd5)
}