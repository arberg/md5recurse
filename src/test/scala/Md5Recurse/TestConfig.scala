package Md5Recurse

import java.io.{File, IOException, PrintWriter}

import org.scalatest._
import scalax.file.Path
// http://jesseeichar.github.io/scala-io-doc/0.4.3/index.html#!/file/string_to_file
//import scalax.file.defaultfs.DefaultPath
//import scalax.file.ImplicitConversions.jfile2path
//import scalax.file.ImplicitConversions.string2path
//import scalax.file.ImplicitConversions.defaultPath2jfile


trait TestConfig extends FlatSpec with TestHelper {

  val paramOnlyPrintModified = "--only-print-modified"
  val paramDisableFileAttributes = "--disable-file-attributes"
  val paramGlobalDir = "--globaldir"
  val paramGlobalDirShort = "-g"
  val paramGlobalDirRelative = "--globaldir-relative"
  val paramGlobalDirRelativeShort = "-G"
  val MD5DATA_EXT = ".md5data"
  val MD5SUM_EXT = ".md5"

  val TEST_EXECUTION_DIR = "build/testExecution"
  val TEST_EXECUTION_DIR_PATH = Path.fromString(TEST_EXECUTION_DIR)
  val TEST_EXECUTION_DIR_FILE = new File(TEST_EXECUTION_DIR)
  val TEST_EXECUTION_GLOBAL_DIR: String = TEST_EXECUTION_DIR + "/globalDir"
  val TEST_EXECUTION_GLOBAL_DIR_PATH = Path.fromString(TEST_EXECUTION_DIR + "/globalDir")
  val TEST_EXECUTION_GLOBAL_FILE: Path = TEST_EXECUTION_GLOBAL_DIR_PATH / "_global.md5data"
  val SRC_TEST_RES_FILES_DIR = "src/test/res/files"
  private val SRC_TEST_RES_DIR_PATH = Path.fromString(SRC_TEST_RES_FILES_DIR)

  def pathContainsFile(path: Path): Boolean = {
    path.children().flatMap(child => if (child.isDirectory) child.children() else List(child)).exists(_.isFile)
  }

  def createDir(path: Path) {
    if (path.nonExistent) path.createDirectory()
  }

  def cleanTestDir = {
    val (_, filesRemaining) = TEST_EXECUTION_DIR_PATH.deleteRecursively(force = true, continueOnFailure = true) // continueOnFailure because of bad error message
    if (filesRemaining > 0 && pathContainsFile(TEST_EXECUTION_DIR_PATH)) throw new RuntimeException("Unable to clean old dir")
    createDir(TEST_EXECUTION_DIR_PATH)
    createDir(Path.fromString(TEST_EXECUTION_GLOBAL_DIR))
    TEST_EXECUTION_DIR
  }

  /**
    *
    * @return Path to copied test resources
    */
  def copyTestResources: Path = {
    cleanTestDir
    assert(TEST_EXECUTION_DIR_PATH.exists, TEST_EXECUTION_DIR_PATH.toString)
    try {
      val testRes = SRC_TEST_RES_DIR_PATH.copyTo(target = TEST_EXECUTION_DIR_PATH / "testRes", copyAttributes = false, replaceExisting = true)
      deleteMd5FileAttributes(testRes)
      testRes
    } catch {
      case e: java.io.IOException => throw new RuntimeException("Unable to copy test resources", e)
    }
  }

  def writeFile(filename: String, content: String): Unit = {
    new PrintWriter(filename) {
      write(content);
      close
    }
  }

  def writeFile(filename: File, content: String): Unit = {
    new PrintWriter(filename) {
      write(content);
      close
    }
  }
}