package Md5Recurse

import java.io.File

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FlatSpec with Matchers with TestConfig {

  "FileUtil" should "should read/write attributes" in {
    val testDirPath = copyTestResources
    val path = testDirPath / "dummy1.log"
    val f = new File(path.path)

    val name: String = "test"
    val value1: String = "value1"
    FileUtil.setAttr(FileUtil.attrView(f), name, value1)
    FileUtil.getAttr(FileUtil.attrView(f), name) should be (Some(value1))

    val value2: String = "value2"
    FileUtil.setAttr(FileUtil.attrView(f), name, value2)
    FileUtil.getAttr(FileUtil.attrView(f), name) should be (Some(value2))

    FileUtil.deleteAttr(FileUtil.attrView(f), name)
    FileUtil.getAttr(FileUtil.attrView(f), name) should be (None)
  }


}