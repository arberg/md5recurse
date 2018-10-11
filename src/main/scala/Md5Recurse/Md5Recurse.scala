package Md5Recurse

import java.io.{File, FileNotFoundException, PrintWriter}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Calendar

import Util.WordWrap._
import com.twmacinta.util.MD5
import scalax.file.Path
import scalax.io.Codec

import scala.collection.immutable.Seq
import scala.collection.{mutable, _}

// We keep an execution log so our tests can monitor what the program did, when it is difficult to determine by seeing output
object Version {
    var version = "1.0.4"
}

object ExecutionLog {
    var current = new ExecutionLog
}

class ExecutionLog {
    var readFileAndGeneratedMd5 = false
}

object Sys {
    val OS: String = System.getProperty("os.name")
    val isWin: Boolean = OS.startsWith("Windows")
    val currentDateString: String = new SimpleDateFormat("yyyyMMdd_HHmm").format(Calendar.getInstance.getTime)
    val lineSeparator: String = System.lineSeparator() // Unused but might be useful, currently file writing converts \n
}

object Config {
    var it: Config = _
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
                     quiet: Boolean = false,
                     doVerify: Boolean = false,
                     doVerifyModified: Boolean = false,
                     doGenerateMd5ForNewAndExistingNewerFiles: Boolean = true,
                     doPrintMissing: Boolean = false, // requires doGenerateNew=false to work properly
                     doPrintModified: Boolean = false,

                     //  excludePattern : String = ".*/.md5data",
                     disableMd5ForDirFilename: String = ".disable_md5",
                     md5DataGlobalFileName: String = "_global.md5data",
                     md5sumPrDirFilename: String = ".md5",
                     failedVerificationLogFilePostfix: String = "_global_failed.log",
                     failedVerificationFilePostfix: String = "_global_failed.md5data",
                     errorsFilePostfix: String = "_global-errors.md5data",
                     readMd5DataGlobally: Boolean = false,
                     readMd5DataPrDirectory: Boolean = false,
                     writeMd5DataPrDirectory: Boolean = false,
                     useRelativePathsInGlobalFile: Boolean = false,
                     optionGlobalRelative: Boolean = false,
                     optionGlobalNonRelative: Boolean = false,
                     optionLocal: Boolean = false,
                     optionOnlyPrintModified: Boolean = false,
                     alwaysUpdateLocal: Boolean = false,
                     silenceWarnings: Boolean = false,
                     printMd5: Boolean = false,
                     useFileAttributes: Boolean = true) {

    val writeMd5DataGlobally = !doPrintMissing && !doPrintModified && md5dataGlobalFolder.isDefined
    val md5dataGlobalFolderAbsoluteFile = md5dataGlobalFolder.map(_.getAbsoluteFile)
    val md5dataGlobalFolderAbsolutePath = md5dataGlobalFolderAbsoluteFile.map(_.getPath)

    //    def md5DataExtension: String = md5DataPrDirFilename

    def md5SumExtension: String = md5sumPrDirFilename

    def prefixDot: String = (if (md5FilePrefix.isEmpty) "" else ".") + md5FilePrefix

    def md5sumName: String = prefixDot + md5sumPrDirFilename

    // def md5dataName(): String = prefixDot + md5DataPrDirFilename

    def md5dataGlobalName: String = md5FilePrefix + md5DataGlobalFileName

    def md5dataGlobalFolderPathOption: Option[String] = md5dataGlobalFolderAbsolutePath

    def md5dataGlobalFilePath: String = md5dataGlobalFolderAbsolutePath.get + "/" + md5dataGlobalName

    def tmpMd5dataGlobalFileName: String = "temp_" + md5dataGlobalName

    def tmpMd5dataGlobalFilePath: String = md5dataGlobalFolderAbsolutePath.get + "/" + tmpMd5dataGlobalFileName

    def failureFile: File = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + (if (md5FilePrefix.isEmpty) "" else "_") + md5FilePrefix + Config.it.failedVerificationFilePostfix)

    def failureLogFile: File = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + (if (md5FilePrefix.isEmpty) "" else "_") + md5FilePrefix + Config.it.failedVerificationLogFilePostfix)

    def errorFile: File = new File(md5dataGlobalFolder.get.getAbsolutePath + "/" + Sys.currentDateString + md5FilePrefix + Config.it.errorsFilePostfix)
}

// https://github.com/scopt/scopt
//noinspection ScalaUnnecessaryParentheses
class Md5OptionParser extends scopt.OptionParser[Config]("Md5Recurse") {
    val TEXT_WRAP = 100
    val TEXT_INDENT = 27

    def pruneAndWrapText(text: String) = text.replaceAll("\n", "").replaceAll("[ ]+", " ").wordWrap(TEXT_WRAP, TEXT_INDENT)

    head("Md5Recurse", "version " + Version.version)

    note(("Md5Recurse generates MD5 hashes for files recursively within directories or on single files. Data is written to file attributes by default, " +
        "and can also be written with local files in each directory or to a single global file. It is fastest to access a single file, so if enabled md5data will be read from " +
        "global before local, and from local before file attributes. If a file has changed all storage forms will be read, before recalculating MD5 hash of actual file. " +
        "On verify all hash changes will be printed to stderr plus " +
        "written to a file in the global-dir (if global dir defined with -g)").wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

    note("Secret info and fair warning: All directories containing a file by the name '.disable_md5' will be skipped (recursively)".wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

    arg[File]("<dir>...") minOccurs 1 unbounded() action { (x, c) =>
        // Use getCanonicalPath because on Windows filenames are not case sensitive, but its a lot slower so only use it for inputs
        // We convert paths as early as possible so we don't have to convert them every step of the way
        c.copy(srcDirs = c.srcDirs :+ x.getCanonicalFile)
    } text pruneAndWrapText("dirs on which to execute recursive MD5 generation or with --check then recurse MD5 check\n")

    opt[Unit]("reread-all") action { (_, c) =>
        c.copy(doVerify = true)
    } text pruneAndWrapText("force read and regen md5 of existing unmodified files and compare with last recorded md5, and print warning for all mismatching files (will write to stderr or globaldir)")

    opt[Unit]('c', "check") action { (_, c) =>
        c.copy(doVerify = true)
    } text
        pruneAndWrapText(
            """verifies MD5 of existing unmodified files, ie. files with same modification timestamp as the timestamp of the previous hash. Prints
            warning if file content changed, and logs entry with original hash to log-file. As with a regular scan, all new and modified files are also scanned, and
            updated hash values are written to data storage.""")

    opt[Unit]("check-all") action { (_, c) =>
        c.copy(doVerify = true, doVerifyModified = true)
    } text pruneAndWrapText("""same as --check, but verifies MD5 of all files even files with newer timestamps.""")

    opt[Unit]("only-check") action { (_, c) =>
        c.copy(doVerify = true, doGenerateMd5ForNewAndExistingNewerFiles = false)
    } text pruneAndWrapText(
        """same as --check, but does not scan new files nor files with modified timestamp. Updates hash values are written to
            data storage, and the original hashes for modified files with identical timestamps are written to a log file.""")

    opt[Unit]("only-print-missing") action { (_, c) =>
        c.copy(doGenerateMd5ForNewAndExistingNewerFiles = false, doPrintMissing = true)
    } text pruneAndWrapText("only print missing/deleted files based on globaldir set. Local files will not be read. New MD5's will not be generated in this mode, nor will md5data be updated.")

    opt[Unit]("only-print-modified") action { (_, c) =>
        c.copy(doGenerateMd5ForNewAndExistingNewerFiles = false, doPrintModified = true, useFileAttributes = false, optionOnlyPrintModified = true)
    } text pruneAndWrapText("only print modified files based on globaldir set. Local files and fileattributes will not be read. New MD5's will not be generated in this mode, nor will md5data be updated.")

    opt[Unit]("print-modified") action { (_, c) =>
        c.copy(doPrintModified = true)
    } text pruneAndWrapText("print filenames with modified timestamp.".wordWrap(TEXT_WRAP, TEXT_INDENT))

    opt[File]('g', "globaldir") valueName "<dir>" action { (x, c) =>
        c.copy(md5dataGlobalFolder = Some(x), readMd5DataGlobally = true, useRelativePathsInGlobalFile = false, optionGlobalNonRelative = true)
    } text pruneAndWrapText("The directory to store the global MD5 info in a flat file. Note with global enabled missing MD5's will still be read from local in each directory file if such files exists")

    opt[File]('G', "globaldir-relative") valueName "<dir>" action { (x, c) =>
        c.copy(md5dataGlobalFolder = Some(x), readMd5DataGlobally = true, useRelativePathsInGlobalFile = true, optionGlobalRelative = true)
    } text pruneAndWrapText("same as globaldir, but paths will be relative to the directory of this global file")

    opt[Unit]("local") action { (_, c) =>
        c.copy(readMd5DataPrDirectory = true, writeMd5DataPrDirectory = true, optionLocal = true)
    } text pruneAndWrapText("Enable reading and writing .md5 files in each directory")

    opt[Unit]("local-update-all") action { (_, c) =>
        c.copy(alwaysUpdateLocal = true)
    } text pruneAndWrapText("Always updates local files and attributes (when enabled) even if no changes found in files in directory")

    opt[String]('p', "prefix") valueName "<local-file-prefix>" action { (x, c) =>
        c.copy(md5FilePrefix = x, logPostFix = s" ($x)")
    } text pruneAndWrapText("prefix for global and local dir files: The <prefix>.md5data and <prefix>_global.md5data data files for this program and <prefix>.md5 in md5sum-format")

    opt[Unit]("disable-file-attributes") action { (_, c) =>
        c.copy(useFileAttributes = false)
    } text pruneAndWrapText("disable reading and writing user file attributes which saves the MD5 and timestamp of last scan directly in the files attributes".wordWrap(TEXT_WRAP, TEXT_INDENT))

    opt[Unit]("deletemd5") action { (_, c) =>
        c.copy(deleteMd5 = true)
    } text pruneAndWrapText("recursively delete local/pr directory MD5 sum files (both .md5data and .md5sum). All hash-generation is disabled when this option is applied".wordWrap(TEXT_WRAP, TEXT_INDENT))

    opt[Unit]("silence-warnings") action { (_, c) =>
        c.copy(silenceWarnings = true)
    } text pruneAndWrapText("don't print warnings for files which could not be read or where file attributes could not be updated")

    opt[String]('e', "encoding") valueName "<charset>" action { (x, c) =>
        if (x == "UTF-8-BOM") {
            c.copy(encoding = "UTF-8", encodingBom = true)
        } else {
            c.copy(encoding = x, encodingBom = false)
        }
    } text pruneAndWrapText("the charset for the .md5 files for md5sum. This setting will not affect .md5data files. Encodings: UTF-8 (default), " +
        "UTF-8-BOM (UTF-8 with BOM), ISO-8859-1 (see https://docs.oracle.com/javase/7/docs/api/java/nio/charset/Charset.html). Note that many windows programs will need the UTF-8 with BOM to " +
        "correctly parse files, while the linux md5sum program fails to parse the BOM character.")

    opt[Unit]('p', "print") action { (_, c) =>
        c.copy(printMd5 = true)
    } text pruneAndWrapText("print MD5 hashes to stdout")

    def verboseCopy(c: Config, level: Int): Config =
        c.copy(
            logMd5Scans = level % 10 >= 1,
            logMd5ScansAndSkipped = level % 10 >= 2,
            logMd5ScansSkippedAndLocalAndAttributeReads = level % 10 >= 3,
            logMd5ScansSkippedAndPrintDetails = level % 10 >= 4,
            logPerformance = level >= 10
        )

    opt[Unit]('v', "verbose") action { (_, c) =>
        verboseCopy(c, 1)
    } text pruneAndWrapText("verbose level 1")

    opt[Int]('V', "verboselevel") valueName "<level>" action { (x, c) =>
        verboseCopy(c, x)
    } text pruneAndWrapText("set verbose level 0: none (default), 1: print files being read for md5sum, 2: print all files, 3: even more, with +10 log performance (ie. 12 means performance+print all files)")

    opt[Unit]('q', "quiet") action { (_, c) =>
        c.copy(quiet = true)
    } text pruneAndWrapText("don't print missing source dirs")

    help("help") text "prints this help"
    version("version") text "print version"

    checkConfig { c =>
        if (!c.optionGlobalNonRelative || !c.optionGlobalRelative) success else failure("Use only one of --globaldir (-g) and --globaldir-relative (-G)")
    }
    checkConfig { c =>
        // Not sure we need this, but should definitiely prevent updating local files
        if (!c.optionOnlyPrintModified || !c.optionLocal) success else failure("Use of --only-print-modified prevents --local")
    }
    checkConfig { c =>
        if (c.readMd5DataGlobally || c.readMd5DataPrDirectory || c.useFileAttributes || c.deleteMd5) success else failure("Please choose storage to read from")
    }
    checkConfig { c =>
        // read assert logic as !(x => y), thus assert x => y (and x => y is the same as !x || y)
        // if printing or checking then either global md5 must be enabled or src-folder with local read must be enabled
        if (!(c.doPrintMissing || c.doPrintModified || !c.doGenerateMd5ForNewAndExistingNewerFiles) || (c.srcDirs.nonEmpty && c.readMd5DataPrDirectory || c.md5dataGlobalFolder.isDefined)) {
            success
        } else {
            failure("Please specify directories for md5-generation")
        }
    }
    checkConfig { c =>
        if (c.doPrintMissing && c.md5dataGlobalFolder.isEmpty) failure("--only-print-missing requires --globaldir") else success
    }
    checkConfig { c =>
        // read assert logic as !(x => y), thus assert x => y (and x => y is the same as !x || y)
        // if printing or checking then either global md5 must be enabled or src-folder with local read must be enabled
    {
        if (c.doPrintMissing && c.readMd5DataPrDirectory) Console.out.println("WARNING: local dir files will not be read when searching for missing files")
        success
    }
    }
    checkConfig { c =>
        // src dirs may only be empty when printing or checking
        if (c.srcDirs.nonEmpty || !c.doGenerateMd5ForNewAndExistingNewerFiles) success else failure("Please specify directories for md5-generation")
    }
    checkConfig { c =>
        // src dirs may only be empty when printing or checking
        if (c.quiet && c.logMd5Scans) Console.out.println("Quiet and verbose specified at the same time")
        success
    }
}

object Md5Recurse {

    def md5String(s: String): Array[Byte] = {
        MessageDigest.getInstance("MD5").digest(s.getBytes)
    }

    // read file and generate md5sum
    def md5Sum(workingDir: String, path: String): Option[Md5SumInfo] = {
        val filepath = if (new File(path).isAbsolute) path else workingDir + "/" + path
        md5SumFastJava(filepath)
    }

    def initNativeLibrary() {
        if (!MD5.initNativeLibrary()) {
            Console.out.println("WARNING: Native library NOT loaded")
        }
    }

    // read file and generate md5sum
    def md5SumFastJava(filepath: String): Option[Md5SumInfo] = {
        try {
            val hash = MD5.asHex(MD5.getHash(new File(filepath)))
            Some(new Md5SumInfo(hash, true, filepath))
        } catch {
            case e: java.io.IOException => Console.err.println("Unable to read file: " + e.getMessage); Console.err.flush(); None
        }
    }

    // read file and generate md5sum
    def md5Sum(path: String): Option[Md5SumInfo] = md5Sum("", path)

    def writeMd5DataCommon[A](file: File, outObjects: List[A], proc: A => String, encoding: String, writeBOM: Boolean) {
        implicit val codec: Codec = Codec(encoding) // value to be passed to Path on read and write
        try {
            if (outObjects.nonEmpty) {
                // Map tho outObjects to outlines. Also append to first line the UTF-8 BOM character if enabled.
                val outLines: Seq[String] = outObjects.zipWithIndex.map { case (o, i: Int) => (if (i == 0 && writeBOM) "\uFEFF" else "") + proc(o) }
                val path = Path.fromString(file.getPath)

                // Don't sort the read lines, because if user manually edited file it does not hurt rewriting file. If user didn't edit file, it will have identical sorting
                def equals[T](list1: T, list2: T) = {
                    val equals = list1 == list2
                    if (equals) {
                        if (Config.it.logMd5ScansAndSkipped) Console.out.println("Identical local file " + file)
                    }
                    equals
                }

                // Disabled check content changed, because now we instead test timestamps of files, and the timestamp check is not so valuable if we don't update timestamp of equal files. Md5Sum files can be equal even if lastModified of a file changed.
                //        if (!path.exists/* || !equals(outLines, path.lines().toList)*/) {
                if (Config.it.logMd5ScansAndSkipped) Console.out.println("Updating local file " + file)
                try {
                    path.writeStrings(strings = outLines, separator = "\n")
                } catch {
                    case exception: scalax.io.ScalaIOException =>
                        exception.getCause() match {
                            case _: java.nio.charset.UnmappableCharacterException =>
                                Console.out.println(s"WARNING: Character could not be written as with encoding $encoding, file will be written using UTF-8: $file")
                                path.writeStrings(strings = outLines, separator = "\n")(Codec.UTF8)
                            case _ => throw exception
                        }
                    case e: java.io.IOException =>
                        Console.err.println("Unable to write file " + path.path + ": " + e.getMessage)
                        Console.err.flush()
                }
                //        } // end equal
            } else if (file.exists) {
                if (!file.delete()) Console.out.println("Failed to delete " + file)
            }
        } catch {
            case _: scalax.io.ScalaIOException => Console.err.println("Error occurred writing file: " + file)
            case _: FileNotFoundException => Console.err.println("Unable to create file: " + file)
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

    def writeLocalMd5File(dir: File, l: List[Md5FileInfo], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
        writeMd5DataCommon(Config.it.writeMd5DataPrDirectory, Config.it.md5sumName, { x: Md5FileInfo => x.exportMd5Line }, Config.it.encoding, Config.it.encodingBom, dir, l, isFileUpdated, greatestLastModifiedTimestampInDir)
    }

    def printMd5Hashes(dirPath: File, md5s: List[Md5FileInfo]) {
        if (md5s.nonEmpty) {
            // dirPath will be null if single file scan
            if (dirPath != null) Console.out.println(">" + md5s.head.getDirectoryPath)
            for (md5 <- md5s) println(md5.exportMd5Line)
            Console.out.flush()
        }
    }

    def writeTextFile(outFile: File, text: String, append: Boolean) {
        val writer: PrintWriter = new PrintWriter(outFile, "UTF-8")
        if (append) {
            writer.append(text)
        } else {
            writer.print(text)
        }
        writer.close()
    }

    /**
      * Check all files in the given dir. If files timestamp changed then the hash will be computed and updated.
      *
      * @param dir          the parent dir
      * @param globalDirSet map of md5's
      * @return
      */
    //noinspection TypeAnnotation
    def verifyAndGenerateMd5ForDirectoryNonRecursive(dir: File, globalDirSet: Option[Map[String, Md5FileInfo]]) = {
        verifyAndGenerateMd5NonRecursive(dir, FileUtil.listFiles(dir).toList, globalDirSet)
    }

    //noinspection TypeAnnotation
    def verifyAndGenerateMd5SingleFile(file: File, globalDirSet: Option[Map[String, Md5FileInfo]]) = {
        verifyAndGenerateMd5NonRecursive(or(file.getParentFile, new File(".")), List(file), globalDirSet)
    }

    def or(file: File, file2: File): File = {
        if (file != null) file else file2
    }

    def getFromDirSet(dirSet: Option[Map[String, Md5FileInfo]], file: File): Option[Md5FileInfo] = {
        if (dirSet.isDefined) {
            dirSet.get.get(file.getName)
        } else {
            None
        }
    }

    def isMatchingFileLastModified(fileInfo: Option[Md5FileInfo], lastModified: Long): Boolean = {
        fileInfo.isDefined && fileInfo.get.lastModified == lastModified
    }

    def mostRecent(val1: Option[Md5FileInfo], val2: Option[Md5FileInfo]): Option[Md5FileInfo] = {
        if (val1.isEmpty) {
            val2
        } else if (val2.isEmpty) {
            val1
        } else if (val1.get.lastModified < val2.get.lastModified) {
            val2
        } else {
            val1
        }
    }

    def verifyAndGenerateMd5NonRecursive(dir: File, files: List[File], globalDirSet: Option[Map[String, Md5FileInfo]]): (List[Md5FileInfo], List[Md5FileInfo], List[String], Boolean, Long) = {
        val config = Config.it
        //    if (config.verbose >= 2) print("Dir: " + dir)
        // Read md5data files in dir lazily, so we only read it if the global file does not exist or a file has been updated in the directory
        lazy val localDirSet = if (Config.it.readMd5DataPrDirectory) {
            Md5FileInfo.readDirFile(dir.getPath + "/" + Config.it.md5sumName, md5sumWithDataInComment = true)
        } else {
            None
        }
        val failureMsgs = mutable.MutableList[String]()
        val failures = mutable.MutableList[Md5FileInfo]()
        val md5s = mutable.MutableList[Md5FileInfo]()
        var fileCount = 0
        var isFileUpdated = false
        var greatestLastModifiedTimestampInDir = 0l

        // Print missing based on globalDir set
        if (Config.it.doPrintMissing && globalDirSet.isDefined) {
            for ((_, md5FileInfo: Md5FileInfo) <- globalDirSet.get
                 if !new File(dir, md5FileInfo.getFileInfo.getName).exists()) {
                Console.out.println("Missing " + md5FileInfo.getFileInfo.getDirectoryPath() + File.separatorChar + md5FileInfo.getFileInfo.getName)
            }
        }

        // loop files (exclude *.md5)
        val isInGlobalFileDir = Config.it.md5dataGlobalFolderPathOption contains dir.getPath
        for (f <- files if f.isFile;
             name = f.getName
             if !name.endsWith(Config.it.md5SumExtension) if !FileUtil.isSymLink(f) if (!isInGlobalFileDir || name != Config.it.tmpMd5dataGlobalFileName && name != Config.it.md5dataGlobalName)) {

            def generateMd5(isFileSeenBefore: Boolean) = {
                val debugInfo = if (isFileSeenBefore) "Generate " else "New "
                isFileUpdated = true
                val fInfoMd5Option = Md5FileInfo.readFileGenerateMd5Sum(f, config.useFileAttributes)
                if (fInfoMd5Option.isEmpty) {
                    if (config.logMd5Scans) Console.out.println(debugInfo + " " + fInfoMd5Option.get + " Error Reading file")
                    failureMsgs += "Error reading file " + f
                } else {
                    if (config.logMd5Scans) Console.out.println(debugInfo + " " + fInfoMd5Option.get)
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

            //noinspection AccessorLikeMethodIsEmptyParen
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

            fileCount += 1
            greatestLastModifiedTimestampInDir = Math.max(greatestLastModifiedTimestampInDir, f.lastModified())
            val currentFileInfo = FileInfoBasic.create(f)
            val recordedMd5InfoOption: Option[Md5FileInfo] = getNewestMd5FileInfo()
            if (recordedMd5InfoOption.isDefined) {
                if (config.logMd5ScansSkippedAndPrintDetails) {
                    Console.out.println(currentFileInfo.getPath())
                }
                val recordedMd5Info: Md5FileInfo = recordedMd5InfoOption.get
                val recordedFileInfo = recordedMd5Info.getFileInfo
                if (config.logMd5ScansSkippedAndPrintDetails) {
                    Console.out.println("Cur lastMod=" + currentFileInfo.getLastModified() + ", len=" + recordedFileInfo.getSize())
                    Console.out.println("Rec lastMod=" + recordedFileInfo.getLastModified() + ", len=" + recordedFileInfo.getSize() + ", md5=" + recordedMd5Info.md5String)
                }
                val isFileTimestampIdentical = currentFileInfo.getLastModified() == recordedFileInfo.getLastModified()
                if (!isFileTimestampIdentical && Config.it.doPrintModified) Console.out.println("Modified " + f)
                if (isFileTimestampIdentical || Config.it.doVerifyModified) {
                    //  still check if file size change, to generate new md5
                    if (Config.it.doVerify) {
                        val fInfoMd5Option = Md5FileInfo.readFileGenerateMd5Sum(f, config.useFileAttributes)
                        if (fInfoMd5Option.isEmpty) {
                            failureMsgs += "Error reading file " + currentFileInfo
                        } else {
                            val fInfoMd5 = fInfoMd5Option.get
                            if (fInfoMd5.md5String != recordedMd5Info.md5String) {
                                val msgPrefix = if (isFileTimestampIdentical) "" else " but with modified timestamp"
                                val msg = s"Failed verification$msgPrefix: original=" + recordedMd5Info.md5String + " current=" + fInfoMd5.md5String + " " + fInfoMd5.filePath
                                if (!Config.it.quiet) {
                                    Console.err.print(msg)
                                }
                                failureMsgs += msg
                                failures += recordedMd5Info
                            } else {
                                if (config.logMd5Scans) Console.out.println("Verified " + f)
                            }
                            md5s += fInfoMd5
                        }
                    } else {
                        // Old non-verified file
                        if (config.logMd5ScansAndSkipped) Console.out.println("Hash exists with matching file timestamp, file read skipped: " + f)
                        if (config.useFileAttributes && config.alwaysUpdateLocal) Md5FileInfo.updateMd5FileAttribute(f, recordedMd5Info)
                        md5s += recordedMd5Info // timestamp updated on windows NTFS if fileAttributes written
                    }
                } else if (Config.it.doGenerateMd5ForNewAndExistingNewerFiles) {
                    // File with modified lastModified
                    generateMd5(true)
                }
            } else {
                // New file
                if (config.doGenerateMd5ForNewAndExistingNewerFiles) generateMd5(false)
            }
        }
        (md5s.toList, failures.toList, failureMsgs.toList, isFileUpdated, greatestLastModifiedTimestampInDir)
    }

    class DataFileUpdater(config: Config) {
        val globalWriter = new GlobalWriter(config)
        val failureWriter = new FailureWriter(config)
        val pendingMd5sMap = new DirToFileMap // Contains info for single file scans

        def close(): Unit = {
            globalWriter.close()
            failureWriter.close()
        }

        private def sort(md5s: List[Md5FileInfo]) = {
            md5s.sortBy(_.fileName)
        }

        def updateFileSetAndWriteFilesForDirForced(dir: File, unsortedMd5s: List[Md5FileInfo], doFlush: Boolean): Unit = {
            updateFileSetAndWriteFilesForDir(dir, unsortedMd5s, doFlush, isFileUpdated = true, Long.MaxValue)
        }

        def updateFileSetAndWriteFilesForDir(dir: File, unsortedMd5s: List[Md5FileInfo], doFlush: Boolean, isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
            // Sort to make text-comparison of files more useful
            val sortedMd5s = sort(unsortedMd5s)
            if (Config.it.printMd5) printMd5Hashes(dir, sortedMd5s)
            globalWriter.write(dir, sortedMd5s, doFlush)
            writeLocalMd5File(dir, sortedMd5s, isFileUpdated, greatestLastModifiedTimestampInDir)
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
            globalWriter.write(dirFile, sortedMd5s, doFlush = false)
            // Only update local files if there are changes
            if (fileMapperOption.isDefined) writeLocalMd5File(dirFile, sortedMd5s, isFileUpdated = true, Long.MaxValue)
        }

        /**
          * Write the final pending changes. Needed when scanning individual files
          */
        def updateFilesFinalPendingChanges(): Unit = {
            for (pendingDir: String <- pendingMd5sMap.map.keys) {
                updateFileSetAndWriteFilesForDirForced(new File(pendingDir), pendingMd5sMap.removeDir(pendingDir).get.values.toList, doFlush = false)
            }
        }

        def updateFileSetAndWriteFiles(dirOrFile: File, unsortedMd5s: List[Md5FileInfo], failures: List[Md5FileInfo], failureMsgs: List[String], isFileUpdated: Boolean, greatestLastModifiedTimestampInDir: Long) {
            val dir = if (dirOrFile.isDirectory) dirOrFile else dirOrFile.getParentFile
            if (dirOrFile.isDirectory) {
                updateFileSetAndWriteFilesForDir(dirOrFile, unsortedMd5s, doFlush = true, isFileUpdated = isFileUpdated, greatestLastModifiedTimestampInDir)
            } else {
                val fileMapper: FileListOrMap = pendingMd5sMap.getOrCreateDir(dir)
                unsortedMd5s foreach fileMapper.addToMap
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
            if (Config.debugLog) Console.out.println("Writing outside dir: " + dir)
            dataFileUpdater.updateFilesIncludePendingChanges(dir, fileSet.removeDirListMap(dir).get)
        }
        dataFileUpdater.updateFilesFinalPendingChanges()
    }

    def isDirDisabled(dir: File): Boolean = {
        FileUtil.listFiles(dir).exists(f => f.getName.equals(Config.it.disableMd5ForDirFilename))
    }

    /**
      * Used to recursively delete .md5 and .md5data files.
      *
      * Will only delete recursively if parent dir contains file to delete. This is to avoid cost of traversing dir on each invocation
      */
    def recursivelyDeleteLocalMd5WithMessageIfExists(dirPath: Path, filenameToDelete: String): Unit = {
        val md5dataPath = dirPath / filenameToDelete
        if (md5dataPath.exists) {
            for (p <- dirPath ** filenameToDelete) {
                Console.out.println("Deleting local md5sum file '" + p.path + "' due to '" + Config.it.disableMd5ForDirFilename + "' file found")
                p.delete()
            }
        }
    }

    def execVerifyByRecursion(recurse: Boolean, dirOrFile: File, fileSet: DirToFileMap, postScan: (File, List[Md5FileInfo], List[Md5FileInfo], List[String], Boolean, Long) => Unit) {
        if (dirOrFile.exists) {
            if (dirOrFile.isDirectory) {
                val dir = dirOrFile
                val filesInDir = dir.listFiles()
                if (filesInDir == null) {
                    if (!Sys.isWin || !FileUtil.isWinHardLink(dirOrFile)) // Ignore errors on hard links
                    {
                        if (!Config.it.silenceWarnings) Console.err.println("Unable to read dir, permission denied or io error: " + dir.getPath)
                    }
                } else if (!isDirDisabled(dir)) {
                    val (md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir) = verifyAndGenerateMd5ForDirectoryNonRecursive(dir, fileSet.removeDir(dir))
                    postScan(dir, md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir)
                    if (recurse) {
                        for (f <- filesInDir.sortBy(_.getName) if f.isDirectory if !FileUtil.isSymLink(f)) { // Sort traversal to get global files written same order and thus makes text-comparison possible
                            execVerifyByRecursion(recurse = true, f, fileSet, postScan)
                        }
                    }
                } else {
                    if (!Config.it.quiet) Console.out.println(Config.it.disableMd5ForDirFilename + " found - skipped " + dir)
                    val dirPath = Path.fromString(dir.getPath)
                    if (Config.it.writeMd5DataPrDirectory) recursivelyDeleteLocalMd5WithMessageIfExists(dirPath, Config.it.md5sumName)
                }
            } else {
                val file = dirOrFile
                val dir = file.getParentFile
                val (md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir) = verifyAndGenerateMd5SingleFile(file, fileSet.getDir(dir))
                postScan(file, md5s, failureMd5s, failureMessages, isFileUpdated, greatestLastModifiedTimestampInDir)
            }
        } else if (Config.it.doPrintMissing) {
            Console.out.println("MissingDir " + dirOrFile)
            for ((f, _) <- fileSet.getDir(dirOrFile).get) {
                Console.out.println("  " + f)
            }
        }
    }

    def execVerifySrcDirList(recurse: Boolean, dirs: Iterable[String], fileSet: DirToFileMap, dataFileUpdater: DataFileUpdater) {
        for (srcDir <- dirs) {
            // Convert srcDir to real path (not just absolute) to avoid /./ and relative names in global file
            execVerifyByRecursion(recurse, new File(srcDir), fileSet, dataFileUpdater.updateFileSetAndWriteFiles)
        }
    }

    // Should be split into parent with writer ability
    def execute(config: Config) {
        Md5Recurse.initNativeLibrary()
        val dataFileUpdater: DataFileUpdater = new DataFileUpdater(config)
        val fileSet: DirToFileMap =
            if (config.readMd5DataGlobally) {
                Timer.withResult("Md5Recurse.ReadGlobalFile", Config.it.logPerformance) {
                    () => Md5FileInfo.readMd5DataFile(new File(config.md5dataGlobalFilePath), md5sumWithDataInComment = false)
                }
            } else {
                new DirToFileMap()
            }

        if (config.doGenerateMd5ForNewAndExistingNewerFiles || config.srcDirs.nonEmpty) {
            Timer("Md5Recurse.Scan files", config.logPerformance) {
                () => execVerifySrcDirList(recurse = true, config.srcDirs.map(_.getPath()), fileSet, dataFileUpdater)
            }
            Timer("Md5Recurse.printDirsOutsideScope", config.logPerformance) {
                () => printDirsOutsideScope(dataFileUpdater, fileSet, config.srcDirs)
            }
        } else {
            execVerifySrcDirList(recurse = false, fileSet.map.keys, fileSet, dataFileUpdater)
        }
        dataFileUpdater.close()
    }

    def deleteMd5s(config: Config) {
        for (d <- config.srcDirs if d.exists()) {
            FileUtil.traverse(d, {
                f: File =>
                    if (f.getName == config.md5sumName /* || f.getName == config.md5dataName - todo delete */ ) {
                        if (f.delete()) {
                            Console.out.println("Deleted " + f)
                        } else {
                            Console.out.println("Failed to delete " + f)
                        }
                    }
            })
        }
    }

    def main(args: Array[String]): Unit = {
        ExecutionLog.current = new ExecutionLog
        val parser = new Md5OptionParser // parser.parse returns Option[C]
        parser.parse(args, Config()) foreach {
            config =>
                Config.it = config
                FileUtil.silenceReadErrors = config.silenceWarnings
                for (d <- config.srcDirs if !d.exists()) {
                    if (!config.quiet) Console.out.println("Src folder does not exist: " + d)
                }
                if (config.deleteMd5) {
                    deleteMd5s(config)
                } else {
                    if (!config.quiet) {
                        val prefix = "Storage enabled:"
                        if (config.useFileAttributes) {
                            Console.out.println(s"$prefix File attributes")
                        }
                        if (config.readMd5DataPrDirectory) {
                            val postfixLocalStorage = "Local MD5 files pr directory"
                            if (config.writeMd5DataPrDirectory) {
                                Console.out.println(s"$prefix $postfixLocalStorage")
                            } else {
                                Console.out.println(s"$prefix Will read but not update $postfixLocalStorage")
                            }
                        }
                        if (config.readMd5DataGlobally) {
                            val postfixLocalStorage = "Global MD5 data file"
                            if (config.writeMd5DataGlobally) {
                                Console.out.println(s"$prefix $postfixLocalStorage")
                            } else {
                                Console.out.println(s"$prefix Will read but not update $postfixLocalStorage")
                            }
                        }
                    }

                    def paren(str: String) = if (str.isEmpty) "" else s" ($str)"

                    Timer("Md5Recurse" + paren(config.md5FilePrefix), config.logPerformance) {
                        () => execute(config)
                    }
                }
        }
    }

}
