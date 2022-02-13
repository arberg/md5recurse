package Md5Recurse

import java.io.File

import org.scalatest.{Matchers, TestSuite}
import scalax.file.Path

/**
  * Created by Alex on 26-12-2016.
  */
trait TestHelper extends TestSuite with Matchers {

  def replaceMd5LineInFile(md5DataFile: Path, filename: String, newMd5: String) {
    val linesUpdated = replaceMd5Line(md5DataFile.lines().toList, "simple.log", "a" * 32)
    md5DataFile.write(linesUpdated.mkString("\n"))
  }

  /**
    * Finds the line containing filename and replaces its first 32 chars with newMd5
    */
  def replaceMd5Line(md5DataLines: List[String], filename: String, newMd5: String) = {
    md5DataLines.map(line => {
      // Replace MD5
      if (line.endsWith("simple.log")) {
        ("a" * 32) + line.substring(32)
      } else {
        line
      }
    })
  }

  def assertFileNotContains(expectedSubString: String, paths: Path*): Unit = {
    paths foreach (p => assertFilesContainExactly(expectedSubString, 0, p))
  }

  def assertFilesContain(expectedSubString: String, paths: Path*): Unit = {
    paths foreach (p => assert(p.lines().filter(l => l.contains(expectedSubString)).size > 0))
  }

  def assertFilesContainExactlyOnce(expectedSubString: String, paths: Path*): Unit = {
    assertFilesContainExactly(expectedSubString, 1, paths: _*)
  }

  def assertFilesContainExactly(expectedSubString: String, count: Int, paths: Path*): Unit = {
    paths foreach (p => {
      p.lines().foreach(l => println(l))
      assert(p.lines().filter(l => l.contains(expectedSubString)).size === count)
    })
  }

  def validateAttr(path: Path, expectedMd5: String): Unit = {
    validateAttr(new File(path.path), expectedMd5)
  }

  def getAttr(path: Path): Option[String] = {
    FileUtil.getAttr(FileUtil.attrView(new File(path.path)), Md5FileInfo.ATTR_MD5RECURSE)
  }

  def validateAttr(file: File, expectedMd5: String): Unit = {
    val newAttribute = FileUtil.getAttr(FileUtil.attrView(file), Md5FileInfo.ATTR_MD5RECURSE)
    newAttribute.get should not be (None)
    newAttribute.get should include(expectedMd5)
  }

  /**
    * Deletes  md5-hash file attribute for all files in path
    */
  def deleteMd5FileAttributes(path: Path) {
    path.children().foreach(
      p => {
        val lastMod = p.lastModified
        FileUtil.deleteAttr(FileUtil.attrView(new File(p.path)), Md5FileInfo.ATTR_MD5RECURSE)
        assert(lastMod == p.lastModified)
      }
    )
  }

  /**
    * Sets  md5-hash file attribute for all files in path
    */
  def setMd5FileAttributes(path: Path, attributeValue: String) {
    def doIt(p: Path) = FileUtil.setAttr(FileUtil.attrView(new File(p.path)), Md5FileInfo.ATTR_MD5RECURSE, attributeValue)

    if (path.isDirectory)
      path.children().foreach(p => doIt(p))
    else
      doIt(path)
  }

  /**
    * Deletes fileAttribute containing md5-hash for file, then scans the file with MD5TOOL and verifies the new attribute equals expectedMd5
    */
  def md5RecurseFile(filename: String, expectedMd5: Option[String], extraParams: Array[String] = Array()) {
    val file: File = new File(filename).getAbsoluteFile
    file.exists() should be(true)
    val md5FileInfo = Md5FileInfo.readFileGenerateMd5Sum(file, true).get
    if (expectedMd5.isDefined)
      md5FileInfo.md5String should include(expectedMd5.get)

    // delete old file attribute
    FileUtil.deleteAttr(FileUtil.attrView(file), Md5FileInfo.ATTR_MD5RECURSE)
    // scan file
    Md5Recurse.main(extraParams ++ Array(filename))
    // verify
    validateAttr(file, md5FileInfo.md5String)
  }

  def md5RecurseGetOutput(params: Array[String], doEcho: Boolean = true): String = {
    val streamOut = new java.io.ByteArrayOutputStream()
    Console.withOut(streamOut) {
      //all printlns in this block will be redirected
      Md5Recurse.main(params)
    }
    val output = streamOut.toString
    if (doEcho) print(output)
    output
  }

  def md5RecurseGetError(params: Array[String], doEcho: Boolean = true): String = {
    val (_, err) = md5RecurseGetOutputAndError(params, doEcho)
    err
  }

  def md5RecurseGetOutputAndError(params: Array[String], doEcho: Boolean = false): (String, String) = {
    val streamErr = new java.io.ByteArrayOutputStream()
    val streamOut = new java.io.ByteArrayOutputStream()
    Console.withErr(streamErr) {
      Console.withOut(streamOut) {
        //all printlns in this block will be redirected
        Md5Recurse.main(params)
      }
    }
    val output = streamOut.toString
    val error = streamErr.toString
    if (doEcho && output.nonEmpty) println(output)
    if (doEcho && error.nonEmpty) println("stderr: \n" + error + "\n")
    (output, error)
  }

  def md5Recurse(params: Array[String]) {
    Md5Recurse.main(params)
  }

  def md5Recurse(params: String*) {
    println("Running with params: "+params)
    Md5Recurse.main(params.toArray)
  }

  def md5Recurse(param: String) {
    Md5Recurse.main(Array(param))
  }
}
