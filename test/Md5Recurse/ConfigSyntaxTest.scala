package Md5Recurse

import java.io.File

import org.scalatest._

import scalax.file.Path
import scalax.io.Codec

class ConfigSyntaxTest extends FlatSpec with TestConfig with TestData with TestHelper {

  "Config" should "not allow double globaldir" in {
    val (_,err) = md5RecurseGetOutputAndError(Array("--globaldir", "foo", "--globaldir", "foo", "."), true)
    assert(err.contains("Unknown"))
  }

}