package Md5Recurse

import java.io.File
import org.scalatest._

class FileUtilTest extends FlatSpec with Matchers {

  "FileUtil" should "should read/write attributes" in {
    val f = new File("""test-res/files/dummy1.log""")

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