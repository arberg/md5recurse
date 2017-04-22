package Md5Recurse

import java.io.{File, PrintWriter}

import org.scalatest._

import scalax.file.Path
// http://jesseeichar.github.io/scala-io-doc/0.4.3/index.html#!/file/string_to_file
//import scalax.file.defaultfs.DefaultPath
//import scalax.file.ImplicitConversions.jfile2path
//import scalax.file.ImplicitConversions.string2path
//import scalax.file.ImplicitConversions.defaultPath2jfile


trait TestConfig extends FlatSpec with TestHelper {

  val TEST_EXECUTION_DIR = "bin/testExecution"
  val TEST_EXECUTION_DIR_PATH = Path.fromString(TEST_EXECUTION_DIR)
  val TEST_EXECUTION_DIR_FILE = new File(TEST_EXECUTION_DIR)
  val TEST_EXECUTION_GLOBAL_DIR = TEST_EXECUTION_DIR + "/globalDir"
  val SRC_TEST_RES_DIR = "test-res/files"
  private val TEST_RES_DIR_PATH = Path.fromString("test-res/files")

  def pathContainsFile(path : Path) : Boolean = {
    path.children().flatMap(child => if (child.isDirectory) child.children() else List(child)).exists(_.isFile)
  }

  def createDir(path : Path) {
    if (path.nonExistent) path.createDirectory()
  }

  def cleanTestDir = {
    val (_, filesRemaining) = TEST_EXECUTION_DIR_PATH.deleteRecursively(force=true, continueOnFailure = true) // continueOnFailure because of bad error message
    if (filesRemaining > 0 && pathContainsFile(TEST_EXECUTION_DIR_PATH)) throw new RuntimeException("Unable to clean old dir")
    createDir(TEST_EXECUTION_DIR_PATH)
    createDir(Path.fromString(TEST_EXECUTION_GLOBAL_DIR))
    TEST_EXECUTION_DIR
  }

  /**
    *
    * @return Path to copied test resources
    */
  def copyTestResources : Path = {
    cleanTestDir
    assert(TEST_EXECUTION_DIR_PATH.exists, TEST_EXECUTION_DIR_PATH.toString())
    val testRes = TEST_RES_DIR_PATH.copyTo(target = TEST_EXECUTION_DIR_PATH / "testRes", copyAttributes = false, replaceExisting = true)
    deleteMd5FileAttributes(testRes)
    testRes
  }

  def writeFile(filename: String, content: String): Unit = {
    new PrintWriter(filename) { write(content); close }
  }
  def writeFile(filename: File, content: String): Unit = {
    new PrintWriter(filename) { write(content); close }
  }
}