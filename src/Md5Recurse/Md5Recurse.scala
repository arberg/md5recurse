package Md5Recurse

import java.io.{File, FileNotFoundException, PrintWriter}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Calendar

import WordWrap._
import com.twmacinta.util.MD5

import scala.collection._
import scala.collection.immutable.Seq
import scala.collection.mutable.MutableList
import scalax.file.Path
import scalax.io.Codec

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
                   verbose: Integer = 0,
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

  note("Md5Recurse generates MD5 hashes for files recursively within directories or on single files. Data is written to file attributes by default, and can also be written with local files in each directory or to a single global file. On verify all hash changes will be printed to stderr plus written to a file in the global-dir (if global dir defined with -g)".wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

  note("Secret info and fair warning: All directories containing a file by the name '.disable_md5' will be skipped (recursively)".wordWrap(TEXT_WRAP + TEXT_INDENT) + "\n")

  arg[File]("<dir>...") minOccurs (1) unbounded() action { (x, c) =>
    c.copy(srcDirs = c.srcDirs :+ x)
  } text "dirs to execute recursive md5-check on\n"

  opt[Unit]("force") action { (_, c) =>
    c.copy(doVerify = true)
  } text "force read and regen md5 of existing unmodified files and compare with last recorded md5, and print warning for all mismatching files (will write to stderr or globaldir)".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('c', "check") action { (_, c) =>
    c.copy(doVerify = true, doGenerateNew = false)
  } text
    """only check md5 of existing files with same modification timestamp and print
            warning if file content changed, do not scan new files or files with modified timestamp. Output is still written to
            global and/or local md5data files (overwriting input). A file with newer timestamp will not be md5-compared,
            but the new files md5 will be output to md5data file if enabled (permitting manual diff afterwards if
            original is saved elsewhere)""".replaceAll("\n", "").replaceAll("[ ]+", " ").wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("onlyPrintMissing") action { (_, c) =>
    c.copy(doGenerateNew = false, doPrintMissing = true, writeMd5DataGlobally = false)
  } text "only print missing (deleted) files. New md5s will not be generated in this mode, nor will md5data be updated".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('i', "ignoreError") action { (_, c) =>
    c.copy(continueOnError = true)
  } text "continue on errors and log to file"

  opt[File]('g', "globaldir") valueName "<dir>" action { (x, c) =>
    if (!c.doPrintMissing)
      c.copy(md5dataGlobalFolder = Some(x), writeMd5DataGlobally = true, readMd5DataGlobally = true)
    else
      c.copy(md5dataGlobalFolder = Some(x))
  } text "The directory to store the global md5 info in a flat file. Note with global enabled missing md5's will still be read from local in each directory file if such files exists (unless disabled)".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("disableReadGlobalMd5") action { (x, c) =>
    c.copy(readMd5DataGlobally = false)
  } text "Disable reading md5s from global file. Reading the global file uses more memory. This option changes effect of --global <dir>".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("enableLocalMd5Data") action { (x, c) =>
    c.copy(readMd5DataPrDirectory = true, writeMd5DataPrDirectory = true)
  } text "Enable reading and writing Md5Data Pr Directory "

  opt[Unit]("enableLocalMd5Sum") action { (x, c) =>
    c.copy(writeMd5SumPrDirectory = true)
  } text "Enable writing files Pr Directory usable by md5sum, files will not be read by this program"

  opt[String]('p', "prefix") valueName "<local-file-prefix>" action { (x, c) =>
    c.copy(md5FilePrefix = x)
  } text "prefix for global and local dir files .md5 (for md5sum program) and .md5data (data file for this program)"

  opt[Unit]("deletemd5") action { (_, c) =>
    c.copy(deleteMd5 = true)
  } text "recursively delete local/pr directory md5 sum files (both .md5data and .md5sum). All hash-generation is disabled when this option is applied".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[String]('e', "encoding") valueName "<charset>" action { (x, c) =>
    if (x == "UTF-8-BOM") {
      c.copy(encoding = "UTF-8", encodingBom = true)
    } else {
      c.copy(encoding = x)
    }
  } text "the charset for the .md5 files for md5sum. This setting will not affect .md5data files. Encodings: UTF-8 (default), UTF-8-BOM (UTF-8 with BOM), ISO-8859-1 (see https://docs.oracle.com/javase/7/docs/api/java/nio/charset/Charset.html)".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]("disable-file-attributes") action { (_, c) =>
    c.copy(useFileAttributes = false)
  } text "disable reading and writing user file attributes which saves the md5 and timestamp of last scan directly in the files attributes".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('p', "print") action { (_, c) =>
    c.copy(printMd5 = true)
  } text "print md5 hashes to stdout"

  opt[Unit]('v', "verbose") action { (_, c) =>
    c.copy(verbose = 1)
  } text "verbose level 1"

  opt[Int]('V', "verboselevel") valueName "<level>" action { (x, c) =>
    c.copy(verbose = x)
  } text "set verbose level 0: none (default), 1: print files being read for md5sum, 2: print all files, 3: even more".wordWrap(TEXT_WRAP, TEXT_INDENT)

  opt[Unit]('q', "quiet") action { (_, c) =>
    c.copy(quiet = true)
  } text "don't print missing source dirs"

  help("help") text "prints this help"
  version("version") text "print version"

  checkConfig { c =>
    if (c.readMd5DataGlobally || c.readMd5DataPrDirectory || c.useFileAttributes || c.deleteMd5) success else failure("Please choose storage to read from (--disableReadGlobalMd5 requires --enableLocalMd5Data) ")
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

  // TODO md5sum fails on windows with files containing {}. It works if filecontent piped into md5sum.
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

  def writeMd5Data[A](file: File, outObjects: List[A], proc: A => String, encoding: String, writeBOM: Boolean) {
    implicit val codec = Codec(encoding) // value to be passed to Path on read and write
    try {
      if (outObjects.size > 0) {
        // Map tho outObjects to outlines. Also append to first line the UTF-8 BOM character if enabled.
        val outLines: Seq[String] = outObjects.zipWithIndex.map { case (o, i: Int) => (if (i == 0 && writeBOM) "\uFEFF" else "") + proc(o) }
        val path = Path.fromString(file.getPath)
        // Don't sort the read lines, because if user manually edited file it does not hurt rewriting file. If user didn't edit file, it will have identical sorting
        if (!path.exists || outLines != path.lines().toList) {
          try {
            path.writeStrings(strings = outLines, separator = "\n")
          } catch {
            case exception: scalax.io.ScalaIOException => {
              exception.getCause() match {
                case e1: java.nio.charset.UnmappableCharacterException => {
                  println(s"WARNING: Character could not be written as with encoding $encoding, file will be written using UTF-8: $file")
                  path.writeStrings(strings = outLines, separator = "\n")(Codec.UTF8)
                }
                case _ => throw exception
              }
            }
          }
        }
      }
      else if (file.exists) {
        if (!file.delete()) println("Failed to delete " + file)
      }
    } catch {
      case e: scalax.io.ScalaIOException => System.err.println("Error occurred writing file: " + file)
      case e: FileNotFoundException => System.err.println("Unable to create file: " + file)
    }
  }

  def writeMd5Data(dir: File, l: List[Md5FileInfo]) {
    if (Config.it.writeMd5DataPrDirectory) {
      val dataFile: File = new File(dir + "/" + Config.it.md5dataName)
      writeMd5Data(dataFile, l, { x: Md5FileInfo => x.exportDataLineFileName }, "UTF-8", false)
    }
  }

  def writeMd5sumFiles(dir: File, l: List[Md5FileInfo]) {
    if (Config.it.writeMd5SumPrDirectory) {
      val dataFile: File = new File(dir + "/" + Config.it.md5sumName())
      writeMd5Data(dataFile, l, { x: Md5FileInfo => x.exportMd5Line }, Config.it.encoding, Config.it.encodingBom)
    }
  }

  def writeBothMd5Files(dir: File, l: List[Md5FileInfo]) {
    writeMd5Data(dir, l)
    writeMd5sumFiles(dir, l)
  }

  def printMd5Hashes(dirPath: File, md5s: List[Md5FileInfo]) {
    if (md5s.size > 0) {
      // dirPath will be null if single file scan
      if (dirPath != null) {
        println(">" + dirPath.getAbsolutePath);
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
  def verifyAndGenerateMd5ForDirectoryNonRecursive(dir: File, globalDirSet: Option[Map[String, Md5FileInfo]]): (List[Md5FileInfo], List[Md5FileInfo], List[String]) = {
    verifyAndGenerateMd5NonRecursive(dir, dir.listFiles().toList, globalDirSet)
  }

  def verifyAndGenerateMd5SingleFile(file: File, globalDirSet: Option[Map[String, Md5FileInfo]]): (List[Md5FileInfo], List[Md5FileInfo], List[String]) = {
    verifyAndGenerateMd5NonRecursive(or(file.getParentFile, new File(".")), List(file), globalDirSet)
  }

  def or(file: File, file2: File) = {
    if (file != null) file else file2
  }

  def verifyAndGenerateMd5NonRecursive(dir: File, files: List[File], globalDirSet: Option[Map[String, Md5FileInfo]]): (List[Md5FileInfo], List[Md5FileInfo], List[String]) = {
    val config = Config.it
    //    if (config.verbose >= 2) print("Dir: " + dir)
    // Read md5data files in dir
    val dirSet =
    if (Config.it.readMd5DataPrDirectory && (globalDirSet.isEmpty || globalDirSet.get.isEmpty)) {
      Md5FileInfo.readDirFile(dir.getAbsolutePath + "/" + Config.it.md5dataName())
    } else if (globalDirSet.isDefined)
      globalDirSet.get
    else
      new immutable.HashMap[String, Md5FileInfo]()
    val failureMsgs = MutableList[String]()
    val failures = MutableList[Md5FileInfo]()
    val md5s = MutableList[Md5FileInfo]()
    var fileCount = 0

    // Print missing based on globalDir set
    if (Config.it.doPrintMissing && globalDirSet.isDefined)
      for {(_, md5FileInfo: Md5FileInfo) <- globalDirSet.get
           if (!new File(dir, md5FileInfo.getFileInfo().getName).exists())} {
        println("Missing " + md5FileInfo.getFileInfo().getDirectoryPath() + File.separatorChar + md5FileInfo.getFileInfo().getName)
      }

    // loop files (exclude *.md5data and *.md5)
    for (f <- files if f.isFile() if !f.getName.endsWith(Config.it.md5DataExtension()) if !f.getName.endsWith(Config.it.md5SumExtension()) if !FileUtil.isSymLink(f)) {
      def generateMd5(debugInfo: String) = {
        val fInfoMd5Option = Md5FileInfo.readFileGenerateMd5Sum(f, config.useFileAttributes)
        if (!fInfoMd5Option.isDefined) {
          failureMsgs += "Error reading file " + f
        } else {
          if (config.verbose >= 1) println(debugInfo + " " + fInfoMd5Option.get)
          md5s += fInfoMd5Option.get
        }
      }

      def getMd5FileInfoPreferFromAttribute(): Option[Md5FileInfo] = {
        var m: Option[Md5FileInfo] = None
        try {
          if (config.useFileAttributes) m = Md5FileInfo.readMd5FileAttribute(f)
        } catch {
          case e: ParseException => failureMsgs += e.getMessage
        }
        if (m.isDefined) m else dirSet.get(f.getName)
      }

      fileCount += 1
      val currentFileInfo = FileInfoBasic.create(f)
      val recordedMd5InfoOption: Option[Md5FileInfo] = getMd5FileInfoPreferFromAttribute()
      if (recordedMd5InfoOption.isDefined) {
        if (config.verbose >= 3)
          println(currentFileInfo.getPath())
        val recordedMd5Info: Md5FileInfo = recordedMd5InfoOption.get
        val recordedFileInfo = recordedMd5Info.getFileInfo
        if (config.verbose >= 3) {
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
                if (config.verbose >= 1) println("Verified " + f)
              }
              md5s += fInfoMd5
            }
          } else {
            // Old non-verified file
            if (config.verbose >= 2) println("Skipped " + f)
            // update file attribute in case we are switching from global file scheme to local, but don't allow updating timestamp, as actual file content may hove changed since we have not locked file
            if (config.useFileAttributes) Md5FileInfo.updateMd5FileAttribute(f, recordedMd5Info)
            md5s += recordedMd5Info // timestamp updated on windows NTFS if fileAttributes written
          }
        } else {
          // Old modified file
          generateMd5("Generate ")
        }
      } else {
        // New file
        if (config.doGenerateNew) generateMd5("New      ")
      }
    }
    (md5s.toList, failures.toList, failureMsgs.toList)
  }


  // Should be split into parent with writer ability
   def execute(config: Config) {
    Md5Recurse.initNativeLibrary()

    val fileSet =
      if (config.readMd5DataGlobally)
        Md5FileInfo.readFile(new File(config.md5dataGlobalFilePath()))
      else new DirAndFileMap[Md5FileInfo]()
    val globalWriter = new GlobalWriter(config)
    val failureWriter = new FailureWriter(config)

    def updateFileSetAndWriteFiles(dir: File, md5s: List[Md5FileInfo], failures: List[Md5FileInfo], failureMsgs: List[String]) {
      // Sort to make text-comparison of files more useful
      val sortedMd5s = md5s.sortBy(_.fileName())
      val sortedFailures = failures.sortBy(_.fileName())
      if (Config.it.printMd5) printMd5Hashes(dir, sortedMd5s)
      globalWriter.write(dir, sortedMd5s)
      failureWriter.write(dir, sortedFailures, failureMsgs)
      writeBothMd5Files(dir, sortedMd5s)
    }

    def isDirDisabled(dir: File) = {
      val files = dir.listFiles
      if (files != null)
        files.toList.exists(_.getName.equals(".disable_md5"))
      else {
        false // means io error (including permission denied)
      }
    }

    def execVerifyByRecursion(recurse: Boolean, dirOrFile: File, fileSet: DirAndFileMap[Md5FileInfo], postScan: (File, List[Md5FileInfo], List[Md5FileInfo], List[String]) => Unit) {
      if (dirOrFile.exists) {
        if (dirOrFile.isDirectory()) {
          if (dirOrFile.listFiles == null) {
            System.err.println("Unable to read dir, permission denied or io error: " + dirOrFile.getAbsolutePath)
          } else if (!isDirDisabled(dirOrFile)) {
            val (md5s, failureMd5s, failureMessages) = verifyAndGenerateMd5ForDirectoryNonRecursive(dirOrFile, fileSet.getDir(dirOrFile.getAbsoluteFile))
            postScan(dirOrFile, md5s, failureMd5s, failureMessages)
            if (recurse)
              for (f <- dirOrFile.listFiles().sortBy(_.getName).toList if f.isDirectory()) // Sort traversal to get global files written same order and thus makes text-comparison possible
                execVerifyByRecursion(true, f, fileSet, postScan)
          }
        } else {
          val dir = dirOrFile.getParentFile
          val (md5s, failureMd5s, failureMessages) = verifyAndGenerateMd5SingleFile(dirOrFile, fileSet.getDir(dir))
          postScan(dir, md5s, failureMd5s, failureMessages)
        }
      } else if (Config.it.doPrintMissing) {
        println("MissingDir " + dirOrFile)
        for ((f, _) <- fileSet.getDir(dirOrFile).get) {
          println("  " + f)
        }
      }
    }

    def execVerifySrcDirList(recurse: Boolean, dirs: Iterable[File]) {
      for (srcDir <- dirs) {
        // Convert srcDir to real path (not just absolute) to avoid /./ and relative names in global file
        execVerifyByRecursion(recurse, new File(Path(srcDir).toRealPath().path), fileSet, updateFileSetAndWriteFiles)
      }
    }

    def printDirsOutsideScope(fileSet: DirAndFileMap[Md5FileInfo], configSrcDirs: Iterable[File]) {
      for (prevMapDir <- fileSet.map.keys if !configSrcDirs.exists({
        f => (prevMapDir.getAbsolutePath() + File.separator).startsWith(f.getAbsolutePath() + File.separator)
      })) {
        globalWriter.write(prevMapDir, fileSet.getDir(prevMapDir).get.values.toList)
      }
    }

    if (config.doGenerateNew || !config.srcDirs.isEmpty) {
      execVerifySrcDirList(true, config.srcDirs)
      printDirsOutsideScope(fileSet, config.srcDirs)
    } else
      execVerifySrcDirList(false, fileSet.map.keys)
    globalWriter.close();
    failureWriter.close();
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
          execute(config)
        }
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }
  }

}
