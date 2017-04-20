package Md5Recurse

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import scalax.file.Path

/**
  * Created by Alex on 26-12-2016.
  */
trait TestHelper extends FlatSpec with Matchers {
  def validateAttr(path: Path, expectedMd5: String): Unit = {
    validateAttr(new File(path.path), expectedMd5)
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
    path.children().foreach(p =>
      FileUtil.deleteAttr(FileUtil.attrView(new File(p.path)), Md5FileInfo.ATTR_MD5RECURSE))
  }

  /**
    * Deletes fileattribute containing md5-hash for file, then scans the file with MD5TOOL and verifies the new attribute equals expectedMd5
    */
  def md5RecurseFile(filename: String, expectedMd5: Option[String], extraParams : Array[String] = Array()) {
    val file: File = new File(filename)
    file.exists() should be(true)
    val md5FileInfo = Md5FileInfo.createAndGenerate(file).get
    if (expectedMd5.isDefined)
      md5FileInfo.md5String should include(expectedMd5.get)

    // delete old file attribute
    FileUtil.deleteAttr(FileUtil.attrView(file), Md5FileInfo.ATTR_MD5RECURSE)
    // scan file
    Md5Recurse.main(extraParams ++ Array(filename))
    // verify
    validateAttr(file, md5FileInfo.md5String)
  }

  def md5RecurseGetOutput(params : Array[String], doEcho : Boolean = false) : String = {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      //all printlns in this block will be redirected
      Md5Recurse.main(params)
    }
    val output = stream.toString
    if (doEcho) println(output)
    output
  }

  def md5Recurse(params : Array[String]) {
    Md5Recurse.main(params)
  }
}
