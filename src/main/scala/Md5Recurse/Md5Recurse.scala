package Md5Recurse

import java.io.{File, FileNotFoundException, PrintWriter}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Calendar

// IntelliJ frequently deletes the import or changes it:
// import WordWrap._
import WordWrap._
import com.twmacinta.util.MD5

import scala.collection._
import scala.collection.immutable.Seq
import scala.collection.mutable.MutableList
import scalax.file.Path
import scalax.io.Codec

// We keep an execution log so our tests can monitor what the program did, when it is difficult to determine by seeing output
object ExecutionLog {
  var current = new ExecutionLog
}

class ExecutionLog {
  var readFileAndGeneratedMd5 = false
}

object Sys {
  val OS = System.getProperty("os.name")
  val isWin = OS.startsWith("Windows")
  val currentDateString = new SimpleDateFormat("yyyyMMdd_HHmm").format(Calendar.getInstance.getTime)
}

object Config {
  var it: Config = null
  val debugLog = false
}

case class Config(
                   encoding: String = "UTF-8",
                   encodingBom: Boolean = false,
                   md5FilePrefix: String = "",
                   deleteMd5: Boolean = false,
                   srcDirs: List[File] = List(),
                   md5dataGlobalFolder: Option[File] = None, // writeMd5DataGlobally is disabled unless this defined
                   // 0 none
                   // 1 files read with md5s
                   // 2 all files (debug)
                   //                   verbose: Integer = 0,
                   logMd5Scans: Boolean = false,
                   logPerformance: Boolean = false,
                   logMd5ScansAndSkipped: Boolean = false,
                   logMd5ScansSkippedAndLocalAndAttributeReads: Boolean = false,
                   logMd5ScansSkippedAndPrintDetails: Boolean = false,
                   logPostFix: String = "",
                   //  verboseMd5Sum: Boolean = false,
                   quiet: Boolean = false,
                   doVerify: Boolean = false,
                   doGenerateNew: Boolean = true,
                   doPrintMissing: Boolean = false, // requires doGenerateNew=false to work properly

                   //  excludePattern : String = ".*/.md5data",
                   md5DataGlobalFileName: String = "_global.md5data",
                   md5DataPrDirFilename: String = ".md5data",
                   md5sumPrDirFilename: String = ".md5",
                   failedVerificationLogFilePostfix: String = "_global_failed.log",
                   failedVerificationFilePostfix: String = "_global_failed.md5data",
                   errorsFilePostfix: String = "_global-errors.md5data",
                   readMd5DataGlobally: Boolean = false,
                   writeMd5DataGlobally: Boolean = false,
                   readMd5DataPrDirectory: Boolean = false,
                   writeMd5DataPrDirectory: Boolean = false,
                   writeMd5SumPrDirectory: Boolean = false,
                   alwaysUpdateLocal: Boolean = false,
                   continueOnError: Boolean = false,
                   printMd5: Boolean = false,
                   useFileAttributes: Boolean = true) {

  def md5DataExtension() = md5DataPrDirFilename;

  def md5SumExtension() = md5sumPrDirFilename;

  def prefixDot() = (if (md5FilePrefix.isEmpty()) "" else ".") + md5FilePrefix;

  def md5sumName() = prefixDot + md5sumPrDirFilename;

  def md5dataName() = prefixDot + md5DataPrDirFilename;

  def md5dataGlobalName() = md5FilePrefix + md5DataGlobalFileName;

  def md5dataGlobalFilePath() = md5dataGlobalFolder.get.getAbsolutePath + "/" + md5dataGlobalName();

  def tmpMd5dataGlobalFilePath() = md5dataGlobalFolder.get.getAbsolutePath + "/temp_" + md5dataGlobalName();

  def failureFile() = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + (if (md5FilePrefix.isEmpty()) "" else "_") + md5FilePrefix + Config.it.failedVerificationFilePostfix);

  def failureLogFile() = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + (if (md5FilePrefix.isEmpty()) "" else "_") + md5FilePrefix + Config.it.failedVerificationLogFilePostfix);

  def errorFile() = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + md5FilePrefix + Config.it.errorsFilePostfix);
}

// https://github.com/scopt/scopt
//noinspection ScalaUnnecessaryParentheses
class Md5OptionParser extends scopt.OptionParser[Config]("Md5Recurse") {
  val TEXT_WRAP = 100
  val TEXT_INDENT = 27
  head("Md5Recurse", "version 1.0")

  note(("Md5Recurse generates MD5 hashes for files recursively within directories or on single files. Data is written to file attributes by default, " +
    "and can also be written with local files in each directory or to a single global file. It is fastest to access a single file, so if enabled md5data will be read from " +
    "global before local, and from local before file attributes. If a file has changed all storage forms will be read, before recalculating MD5 hash of actual file. " +
    "On verify all hash changes will be printed to stderr plus " +
    "written to a file in the global-dir (if global dir defined with -g)").wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

  note("Secret info and fair warning: All directories containing a file by the name '.disable_md5' will be skipped (recursively)".wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

  arg[File]("<dir>...") minOccurs (1) unbounded() action { (x, c) =>
    // Use getCanonicalPath because on Windows filenames are not case sensitive, but its a lot slower so only use it for inputs
    // We convert paths as early as possible so we don't have to convert them every step of the way
    c.copy(srcDirs = c.srcDirs :+ x.getCanonicalFile)
  } text "dirs on which to execute recursive MD5 generation or with --check then recurse MD5 check\n"

  opt[Unit]("force") action { (_, c) =>
    c.copy(doVerify = true)
  } text "force read and regen md5 of existing unmodified files and compare with last recorded md5, and print warning for all mismatching files (will write to stderr or globaldir)".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('c', "check") action { (_, c) =>
    c.copy(doVerify = true, doGenerateNew = false)
  } text
    """only check MD5 of existing files with same modification timestamp and print
            warning if file content changed, do not scan new files or files with modified timestamp. Output is still written to
            global and/or local md5data files (overwriting input). A file with newer timestamp will not be MD5-compared,
            but the new files md5 will be output to md5data file if enabled (permitting manual diff afterwards if
            original is saved elsewhere)""".replaceAll("\n", "").replaceAll("[ ]+", " ").wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("onlyPrintMissing") action { (_, c) =>
    c.copy(doGenerateNew = false, doPrintMissing = true, writeMd5DataGlobally = false)
  } text "only print missing (deleted) files. New MD5's will not be generated in this mode, nor will md5data be updated".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[File]('g', "globaldir") valueName "<dir>" action { (x, c) =>
    if (!c.doPrintMissing)
      c.copy(md5dataGlobalFolder = Some(x), writeMd5DataGlobally = true, readMd5DataGlobally = true)
    else
      c.copy(md5dataGlobalFolder = Some(x))
  } text "The directory to store the global MD5 info in a flat file. Note with global enabled missing MD5's will still be read from local in each directory file if such files exists".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("enableLocalMd5Data") action { (x, c) =>
    c.copy(readMd5DataPrDirectory = true, writeMd5DataPrDirectory = true)
  } text "Enable reading and writing Md5Data files locally in each directory "

  opt[Unit]("enableLocalMd5Sum") action { (x, c) =>
    c.copy(writeMd5SumPrDirectory = true)
  } text "Enable writing md5sum files locally in each directory. These files will not be read by this program for MD5-data."

  opt[Unit]("alwaysUpdateLocal") action { (x, c) =>
    c.copy(alwaysUpdateLocal = true)
  } text "Always updates local files and attributes (when enabled) even if no changes found in files in directory"

  opt[String]('p', "prefix") valueName "<local-file-prefix>" action { (x, c) =>
    c.copy(md5FilePrefix = x, logPostFix = s" ($x)")
  } text "prefix for global and local dir files: The <prefix>.md5data and <prefix>_global.md5data data files for this program and <prefix>.md5 in md5sum-format"

  opt[Unit]("disable-file-attributes") action { (_, c) =>
    c.copy(useFileAttributes = false)
  } text "disable reading and writing user file attributes which saves the MD5 and timestamp of last scan directly in the files attributes".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("deletemd5") action { (_, c) =>
    c.copy(deleteMd5 = true)
  } text "recursively delete local/pr directory MD5 sum files (both .md5data and .md5sum). All hash-generation is disabled when this option is applied".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('i', "ignoreError") action { (_, c) =>
    c.copy(continueOnError = true)
  } text "continue on errors and log to file"

  opt[String]('e', "encoding") valueName "<charset>" action { (x, c) =>
    if (x == "UTF-8-BOM") {
      c.copy(encoding = "UTF-8", encodingBom = true)
    } else {
      c.copy(encoding = x, encodingBom = false)
    }
  } text "the charset for the .md5 files for md5sum. This setting will not affect .md5data files. Encodings: UTF-8 (default), UTF-8-BOM (UTF-8 with BOM), ISO-8859-1 (see https://docs.oracle.com/javase/7/docs/api/java/nio/charset/Charset.html). Note that many windows programs will need the UTF-8 with BOM to correctly parse files, while the linux md5sum program fails to parse the BOM character.".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('p', "print") action { (_, c) =>
    c.copy(printMd5 = true)
  } text "print MD5 hashes to stdout"

  def verboseCopy(c: Config, level: Int) =
    c.copy(
      logMd5Scans = level % 10 >= 1,
      logMd5ScansAndSkipped = level % 10 >= 2,
      logMd5ScansSkippedAndLocalAndAttributeReads = level % 10 >= 3,
      logMd5ScansSkippedAndPrintDetails = level % 10 >= 4,
      logPerformance = level >= 10
    )

  opt[Unit]('v', "verbose") action { (_, c) =>
    verboseCopy(c, 1)
  } text "verbose level 1"

  opt[Int]('V', "verboselevel") valueName "<level>" action { (x, c) =>
    verboseCopy(c, x)
  } text "set verbose level 0: none (default), 1: print files being read for md5sum, 2: print all files, 3: even more, with +10 log performance (ie. 12 means performance+print all files)".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('q', "quiet") action { (_, c) =>
    c.copy(quiet = true)
  } text "don't print missing source dirs"

  help("help") text "prints this help"
  version("version") text "print version"

  checkConfig { c =>
    if (c.readMd5DataGlobally || c.readMd5DataPrDirectory || c.useFileAttributes || c.deleteMd5) success else failure("Please choose storage to read from")
  }
  checkConfig { c =>
    // read assert logic as !(x => y), thus assert x => y (and x => y is the same as !x || y)
    // if printing or checking then either global md5 must be enabled or src-folder with local read must be enabled
    if (!(c.doPrintMissing || !c.doGenerateNew) || (!c.srcDirs.isEmpty && c.readMd5DataPrDirectory || c.md5dataGlobalFolder.isDefined)) success else failure("Please specify directories for md5-generation")
  }
  checkConfig { c =>
    // src dirs may only be empty when printing or checking
    if (!c.srcDirs.isEmpty || !c.doGenerateNew) success else failure("Please specify directories for md5-generation")
  }
}

object Md5Recurse {

  def md5String(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  // read file and generate md5sum
  def md5Sum(workingDir: String, path: String): Option[Md5SumInfo] = {
    val filepath = if (new File(path).isAbsolute) path else workingDir + "/" + path
    md5SumFastJava(filepath)
  }

  def initNativeLibrary() {
    if (!MD5.initNativeLibrary()) {
      System.out.println("WARNING: Native library NOT loaded");
    }
  }

  // read file and generate md5sum
  def md5SumFastJava(filepath: String): Option[Md5SumInfo] = {
    try {
      val hash = MD5.asHex(MD5.getHash(new File(filepath)))
      Some(new Md5SumInfo(hash, true, filepath))
    } catch {
      case e: java.io.IOException => System.err.println("Unable to read file: " + e.getMessage); System.err.flush(); None
    }
  }

  // read file and generate md5sum
  def md5Sum(path: String): Option[Md5SumInfo] = md5Sum("", path)

  def writeMd5DataCommon[A](file: File, outObjects: List[A], proc: A => String, encoding: String, writeBOM: Boolean) {
    implicit val codec = Codec(encoding) // value to be passed to Path on read and write
    try {
      if (outObjects.size > 0) {
        // Map tho outObjects to outlines. Also append to first line the UTF-8 BOM character if enabled.
        val outLines: Seq[String] = outObjects.zipWithIndex.map { case (o, i: Int) => (if (i == 0 && writeBOM) "\uFEFF" else "") + proc(o) }
        val path = Path.fromString(file.getPath)

        // Don't sort the read lines, because if user manually edited file it does not hurt rewriting file. If user didn't edit file, it will have identical sorting
        def equals[T](list1: T, list2: T) = {
          val equals = list1 == list2
          if (equals) {
            if (Config.it.logMd5ScansAndSkipped) println("Identical local file " + file)
          }
          equals
        }

        // Disabled check content changed, because now we instead test timestamps of files, and the timestamp check is not so valuable if we don't update timestamp of equal files. Md5Sum files can be equal even if lastModified of a file changed.
        //        if (!path.exists/* || !equals(outLines, path.lines().toList)*/) {
          if (Config.it.logMd5ScansAndSkipped) println("Updating local file " + file)
          try {
            path.writeStrings(strings = outLines, separator = "\n")
          } catch {
            case exception: scalax.io.ScalaIOException =>
              exception.getCause() match {
                case e1: java.nio.charset.UnmappableCharacterException => {
                  println(s"WARNING: Character could not be written as with encoding $encoding, file will be written using UTF-8: $file")
                  path.writeStrings(strings = outLines, separator = "\n")(Codec.UTF8)
                }
                case _ => throw exception
              }
            case e: java.io.IOException =>
              System.err.println("Unable to write file " + path.path + ": " + e.getMessage)
              System.err.flush()
          }
          //        } // end equal
      } else if (file.exists) {
        if (!file.delete()) println("Failed to delete " + file)
      }
    } catch {
      case e: scalax.io.ScalaIOException => System.err.println("Error occurred writing file: " + file)
      case e: FileNotFoundException => System.err.println("Unable to create file: " + file)
    }
  }

  def writeMd5DataCommon(configEnabled: Boolean, md5FileName: String, proc: Md5FileInfo => String, encoding: String, writeBOM: Boolean, dir: File, l: List[Md5FileInfo], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
    if (configEnabled) {
      val dataFile: File = new File(dir + "/" + md5FileName)
      if (isFileUpdated || dataFile.lastModified() < greatestLastModifiedTimestampInDir || Config.it.alwaysUpdateLocal || l.isEmpty) {
        writeMd5DataCommon(dataFile, l, proc, encoding, writeBOM)
      }
    }
  }

  def writeMd5Data(dir: File, l: List[Md5FileInfo], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long): Unit = {
    writeMd5DataCommon(Config.it.writeMd5DataPrDirectory, Config.it.md5dataName, { x: Md5FileInfo => x.exportDataLineFileName }, "UTF-8", false, dir, l, isFileUpdated, greatestLastModifiedTimestampInDir)
  }

  def writeMd5sumFiles(dir: File, l: List[Md5FileInfo], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long): Unit = {
    writeMd5DataCommon(Config.it.writeMd5SumPrDirectory, Config.it.md5sumName, { x: Md5FileInfo => x.exportMd5Line }, Config.it.encoding, Config.it.encodingBom, dir, l, isFileUpdated, greatestLastModifiedTimestampInDir)
  }

  def writeBothMd5Files(dir: File, l: List[Md5FileInfo], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
    writeMd5Data(dir, l, isFileUpdated, greatestLastModifiedTimestampInDir)
    writeMd5sumFiles(dir, l, isFileUpdated, greatestLastModifiedTimestampInDir)
  }

  def printMd5Hashes(dirPath: File, md5s: List[Md5FileInfo]) {
    if (md5s.size > 0) {
      // dirPath will be null if single file scan
      if (dirPath != null) {
        println(">" + md5s(0).getDirectoryPath());
      }
      for (md5 <- md5s) {
        println(md5.exportMd5Line);
      }
      System.out.flush
    }
  }

  def writeTextFile(outFile: File, text: String, append: Boolean) {
    val writer: PrintWriter = new PrintWriter(outFile, "UTF-8");
    if (append)
      writer.append(text);
    else
      writer.print(text);
    writer.close();
  }

  /**
    * Check all files in the given dir. If files timestamp changed then the hash will be computed and updated.
    *
    * @param dir
    * @param globalDirSet map of md5's
    * @return
    */
  def verifyAndGenerateMd5ForDirectoryNonRecursive(dir: File, globalDirSet: Option[Map[String, Md5FileInfo]]) = {
    verifyAndGenerateMd5NonRecursive(dir, dir.listFiles().toList, globalDirSet)
  }

  def verifyAndGenerateMd5SingleFile(file: File, globalDirSet: Option[Map[String, Md5FileInfo]]) = {
    verifyAndGenerateMd5NonRecursive(or(file.getParentFile, new File(".")), List(file), globalDirSet)
  }

  def or(file: File, file2: File) = {
    if (file != null) file else file2
  }

  def getFromDirSet(dirSet: Option[Map[String, Md5FileInfo]], file: File): Option[Md5FileInfo] = {
    if (dirSet.isDefined) {
      dirSet.get.get(file.getName)
    } else None
  }

  def isMatchingFileLastModified(fileInfo: Option[Md5FileInfo], lastModified: Long) = {
    fileInfo.isDefined && fileInfo.get.lastModified() == lastModified
  }

  def mostRecent(val1: Option[Md5FileInfo], val2: Option[Md5FileInfo]): Option[Md5FileInfo] = {
    if (!val1.isDefined)
      val2
    else if (!val2.isDefined)
      val1
    else if (val1.get.lastModified() < val2.get.lastModified())
      val2
    else
      val1
  }

  def verifyAndGenerateMd5NonRecursive(dir: File, files: List[File], globalDirSet: Option[Map[String, Md5FileInfo]]): (List[Md5FileInfo], List[Md5FileInfo], List[String], Boolean, Long) = {
    val config = Config.it
    //    if (config.verbose >= 2) print("Dir: " + dir)
    // Read md5data files in dir lazily, so we only read it if the global file does not exist or a file has been updated in the directory
    lazy val localDirSet =
    if (Config.it.readMd5DataPrDirectory) {
      val md5dataFilename = dir.getPath + "/" + Config.it.md5dataName()
      if (Config.it.logMd5ScansSkippedAndLocalAndAttributeReads) println("Reading local md5data: " + md5dataFilename)
      Md5FileInfo.readDirFile(md5dataFilename)
    } else None
    val failureMsgs = MutableList[String]()
    val failures = MutableList[Md5FileInfo]()
    val md5s = MutableList[Md5FileInfo]()
    var fileCount = 0
    var isFileUpdated = false
    var greatestLastModifiedTimestampInDir = 0l

    // Print missing based on globalDir set
    if (Config.it.doPrintMissing && globalDirSet.isDefined)
      for {(_, md5FileInfo: Md5FileInfo) <- globalDirSet.get
           if (!new File(dir, md5FileInfo.getFileInfo().getName).exists())} {
        println("Missing " + md5FileInfo.getFileInfo().getDirectoryPath() + File.separatorChar + md5FileInfo.getFileInfo().getName)
      }

    // loop files (exclude *.md5data and *.md5)
    for (f <- files if f.isFile() if !f.getName.endsWith(Config.it.md5DataExtension()) if !f.getName.endsWith(Config.it.md5SumExtension()) if !FileUtil.isSymLink(f)) {

      def generateMd5(debugInfo: String) = {
        isFileUpdated = true
        val fInfoMd5Option = Md5FileInfo.readFileGenerateMd5Sum(f, config.useFileAttributes)
        if (!fInfoMd5Option.isDefined) {
          failureMsgs += "Error reading file " + f
        } else {
          if (config.logMd5Scans) println(debugInfo + " " + fInfoMd5Option.get)
          md5s += fInfoMd5Option.get
        }
      }

      def readAttribute(file: File) = {
        try {
          if (config.useFileAttributes) Md5FileInfo.readMd5FileAttribute(f) else None
        } catch {
          case e: ParseException => failureMsgs += e.getMessage
            None
        }
      }

      def getNewestMd5FileInfo(): Option[Md5FileInfo] = {
        val lastModified = f.lastModified()
        val fromGlobal = getFromDirSet(globalDirSet, f)
        if (isMatchingFileLastModified(fromGlobal, lastModified)) {
          fromGlobal
        } else {
          val fromLocal = getFromDirSet(localDirSet, f)
          if (isMatchingFileLastModified(fromLocal, lastModified)) {
            fromLocal
          } else {
            val fromAttribute = readAttribute(f)
            if (isMatchingFileLastModified(fromAttribute, lastModified)) {
              fromAttribute
            } else {
              mostRecent(fromLocal, mostRecent(fromGlobal, fromAttribute))
            }
          }
        }
      }

      greatestLastModifiedTimestampInDir = Math.max(greatestLastModifiedTimestampInDir, f.lastModified())
      fileCount += 1
      val currentFileInfo = FileInfoBasic.create(f)
      val recordedMd5InfoOption: Option[Md5FileInfo] = getNewestMd5FileInfo()
      if (recordedMd5InfoOption.isDefined) {
        if (config.logMd5ScansSkippedAndPrintDetails)
          println(currentFileInfo.getPath())
        val recordedMd5Info: Md5FileInfo = recordedMd5InfoOption.get
        val recordedFileInfo = recordedMd5Info.getFileInfo
        if (config.logMd5ScansSkippedAndPrintDetails) {
          println("Cur lastMod=" + currentFileInfo.getLastModified() + ", len=" + recordedFileInfo.getSize())
          println("Rec lastMod=" + recordedFileInfo.getLastModified() + ", len=" + recordedFileInfo.getSize() + ", md5=" + recordedMd5Info.md5String)
        }
        if (currentFileInfo.getLastModified() == recordedFileInfo.getLastModified()) {
          //  still check if filesize change, to generate new md5
          if (Config.it.doVerify) {
            val fInfoMd5Option = Md5FileInfo.readFileGenerateMd5Sum(f, config.useFileAttributes)
            if (!fInfoMd5Option.isDefined) {
              failureMsgs += "Error reading file " + currentFileInfo
            } else {
              val fInfoMd5 = fInfoMd5Option.get
              if (fInfoMd5.md5String != recordedMd5Info.md5String) {
                val msg = "Failed verification: original=" + recordedMd5Info.md5String + " current=" + fInfoMd5.md5String + " " + fInfoMd5.filePath
                if (!Config.it.quiet)
                  System.err.println(msg)
                failureMsgs += msg
                failures += recordedMd5Info
              } else {
                if (config.logMd5Scans) println("Verified " + f)
              }
              md5s += fInfoMd5
            }
          } else {
            // Old non-verified file
            if (config.logMd5ScansAndSkipped) println("Skipped " + f)
            if (config.useFileAttributes && config.alwaysUpdateLocal) Md5FileInfo.updateMd5FileAttribute(f, recordedMd5Info)
            md5s += recordedMd5Info // timestamp updated on windows NTFS if fileAttributes written
          }
        } else {
          // File with modified lastModified
          generateMd5("Generate ")
        }
      } else {
        // New file
        if (config.doGenerateNew) generateMd5("New      ")
      }
    }
    (md5s.toList, failures.toList, failureMsgs.toList, isFileUpdated, greatestLastModifiedTimestampInDir)
  }

  class DataFileUpdater(config: Config) {
    val globalWriter = new GlobalWriter(config)
    val failureWriter = new FailureWriter(config)
    val pendingMd5sMap = new DirToFileMap // Contains info for single file scans

    def close(): Unit = {
      globalWriter.close();
      failureWriter.close();
    }

    private def sort(md5s: List[Md5FileInfo]) = {
      md5s.sortBy(_.fileName())
    }

    def updateFileSetAndWriteFilesForDirForced(dir: File, md5s: List[Md5FileInfo], doFlush: Boolean): Unit = {
      updateFileSetAndWriteFilesForDir(dir, md5s, doFlush, true, Long.MaxValue)
    }

    def updateFileSetAndWriteFilesForDir(dir: File, md5s: List[Md5FileInfo], doFlush: Boolean, isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
      // Sort to make text-comparison of files more useful
      val sortedMd5s = sort(md5s)
      if (Config.it.printMd5) printMd5Hashes(dir, sortedMd5s)
      globalWriter.write(dir, md5s, doFlush)
      writeBothMd5Files(dir, md5s, isFileUpdated, greatestLastModifiedTimestampInDir)
    }

    def updateFilesIncludePendingChanges(dir: String, originalGlobalFileListMap: FileListOrMap): Unit = {
      originalGlobalFileListMap.fillMap(dir)
      val orgMap = originalGlobalFileListMap.map
      val fileMapperOption: Option[Map[String, Md5FileInfo]] = pendingMd5sMap.removeDir(dir)
      // map ++ has new md5s on the right so it overwrites old values
      val updatedFileMap = if (fileMapperOption.isDefined) orgMap ++ fileMapperOption.get else orgMap
      // Always sort because the global file may be the concatenation of other md5data global files done my user (UnRaid scripts)
      val sortedMd5s = sort(updatedFileMap.values.toList)
      val dirFile = new File(dir)
      globalWriter.write(dirFile, sortedMd5s, false)
      // Only update local files if there are changes
      if (fileMapperOption.isDefined)
        writeBothMd5Files(dirFile, sortedMd5s, true, Long.MaxValue)
    }

    /**
      * Write the final pending changes. Needed when scanning individual files
      */
    def updateFilesFinalPendingChanges(): Unit = {
      for (pendingDir: String <- pendingMd5sMap.map.keys) {
        updateFileSetAndWriteFilesForDirForced(new File(pendingDir), pendingMd5sMap.removeDir(pendingDir).get.values.toList, false)
      }
    }

    def updateFileSetAndWriteFiles(dirOrFile: File, md5s: List[Md5FileInfo], failures: List[Md5FileInfo], failureMsgs: List[String], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
      val dir = if (dirOrFile.isDirectory) dirOrFile else dirOrFile.getParentFile
      if (dirOrFile.isDirectory) {
        updateFileSetAndWriteFilesForDir(dirOrFile, md5s, true, isFileUpdated, greatestLastModifiedTimestampInDir)
      } else {
        val fileMapper: FileListOrMap = pendingMd5sMap.getOrCreateDir(dir)
        md5s foreach fileMapper.addToMap
      }
      val sortedFailures = sort(failures)
      failureWriter.write(dir, sortedFailures, failureMsgs)
    }
  }

  def printDirsOutsideScope(dataFileUpdater: DataFileUpdater, fileSet: DirToFileMap, configSrcDirs: Iterable[File]) {
    for (
      dir <- fileSet.map.keySet
      if !configSrcDirs.exists({
        f => (dir + File.separator).startsWith(f.getPath + File.separator)
      })
    ) {
      // If I evaluate dir inside for-loop with ';dir = dirMap._1' then the ordering of the traversal changes
      if (Config.debugLog) println("Writing outside dir: " + dir)
      dataFileUpdater.updateFilesIncludePendingChanges(dir, fileSet.removeDirListMap(dir).get)
    }
    dataFileUpdater.updateFilesFinalPendingChanges()
  }

  // Should be split into parent with writer ability
  def execute(config: Config) {
    Md5Recurse.initNativeLibrary()
    val dataFileUpdater = new DataFileUpdater(config)
    val fileSet: DirToFileMap =
      if (config.readMd5DataGlobally)
        Timer.withResult("Md5Recurse.ReadGlobalFile", Config.it.logPerformance) {
          () => Md5FileInfo.readMd5DataFile(new File(config.md5dataGlobalFilePath()))
        }
      else
        new DirToFileMap()

    def isDirDisabled(dir: File) = {
      val files = dir.listFiles
      if (files != null)
        files.toList.exists(_.getName.equals(".disable_md5"))
      else {
        false // means io error (including permission denied)
      }
    }

    def execVerifyByRecursion(recurse: Boolean, dirOrFile: File, fileSet: DirToFileMap, postScan: (File, List[Md5FileInfo], List[Md5FileInfo], List[String], Boolean, Long) => Unit) {
      if (dirOrFile.exists) {
        if (dirOrFile.isDirectory()) {
          val dir = dirOrFile
          if (dir.listFiles == null) {
            System.err.println("Unable to read dir, permission denied or io error: " + dir.getPath)
          } else if (!isDirDisabled(dir)) {
            val (md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir) = verifyAndGenerateMd5ForDirectoryNonRecursive(dir, fileSet.removeDir(dir))
            postScan(dir, md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir)
            if (recurse)
              for (f <- dir.listFiles().sortBy(_.getName).toList if f.isDirectory()) // Sort traversal to get global files written same order and thus makes text-comparison possible
                execVerifyByRecursion(true, f, fileSet, postScan)
          }
        } else {
          val file = dirOrFile
          val dir = file.getParentFile
          val (md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir) = verifyAndGenerateMd5SingleFile(file, fileSet.getDir(dir))
          postScan(file, md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir)
        }
      } else if (Config.it.doPrintMissing) {
        println("MissingDir " + dirOrFile)
        for ((f, _) <- fileSet.getDir(dirOrFile).get) {
          println("  " + f)
        }
      }
    }

    def execVerifySrcDirList(recurse: Boolean, dirs: Iterable[String]) {
      for (srcDir <- dirs) {
        // Convert srcDir to real path (not just absolute) to avoid /./ and relative names in global file
        execVerifyByRecursion(recurse, new File(srcDir), fileSet, dataFileUpdater.updateFileSetAndWriteFiles)
      }
    }

    if (config.doGenerateNew || !config.srcDirs.isEmpty) {
      Timer("Md5Recurse.Scan files", config.logPerformance) {
        () => execVerifySrcDirList(true, config.srcDirs.map(_.getPath()))
      }
      Timer("Md5Recurse.printDirsOutsideScope", config.logPerformance) {
        () => printDirsOutsideScope(dataFileUpdater, fileSet, config.srcDirs)
      }
    } else
      execVerifySrcDirList(false, fileSet.map.keys)
    dataFileUpdater.close()
  }

  def deleteMd5s(config: Config) = {
    for (d <- config.srcDirs if d.exists())
      FileUtil.traverse(d, {
        f: File =>
          if (f.getName == config.md5sumName() || f.getName == config.md5dataName()) {
            f.delete()
            println("Deleted " + f)
          }
      })
  }

  def main(args: Array[String]): Unit = {
    ExecutionLog.current = new ExecutionLog
    val parser = new Md5OptionParser // parser.parse returns Option[C]
    parser.parse(args, Config()) map {
      config =>
        Config.it = config
        for (d <- config.srcDirs if !d.exists()) {
          if (!config.quiet) println("Src folder does not exist: " + d)
        }
        if (config.deleteMd5) {
          deleteMd5s(config)
        } else {
          if (!config.quiet) {
            val prefix = "Storage enabled:"
            if (config.useFileAttributes) {
              println(s"$prefix File attributes")
            }
            if (config.readMd5DataPrDirectory) {
              val postfixLocalStorage = "Local MD5 data files pr directory"
              if (config.writeMd5DataPrDirectory) {
                println(s"$prefix $postfixLocalStorage")
              } else {
                println(s"$prefix Will read but not update $postfixLocalStorage")
              }
            }
            if (config.readMd5DataGlobally) {
              val postfixLocalStorage = "Global MD5 data file"
              if (config.writeMd5DataGlobally) {
                println(s"$prefix $postfixLocalStorage")
              } else {
                println(s"$prefix Will read but not update $postfixLocalStorage")
              }
            }
          }

          def paren(str: String) = if (str.isEmpty) "" else s" ($str)"

          Timer("Md5Recurse" + paren(config.md5FilePrefix), config.logPerformance) {
            () => execute(config)
          }
        }
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }
  }

}
