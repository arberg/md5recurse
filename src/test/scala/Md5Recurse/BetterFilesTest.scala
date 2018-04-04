package Md5Recurse

import org.scalatest._
// http://jesseeichar.github.io/scala-io-doc/0.4.3/index.html#!/file/string_to_file

class BetterFilesTest extends FlatSpec with Matchers {

    "Better-Files relativize" should "map same dir to ." in {
        val f = better.files.File("/home")
        val relative = f.relativize(f)
        (relative.toString == ".") should be(false)
    }

}