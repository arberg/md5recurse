package Md5Recurse

import org.scalatest._

class ConfigSyntaxTest extends FlatSpec with TestConfig with TestData with TestHelper {

  "Config" should "not allow double globaldir" in {
    val (_,err) = md5RecurseGetOutputAndError(Array("--globaldir", "foo", "--globaldir", "foo", "."), true)
    assert(err.contains("Unknown"))
  }

}