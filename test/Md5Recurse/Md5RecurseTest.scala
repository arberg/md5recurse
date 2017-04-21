package Md5Recurse

import java.io.{File, PrintWriter}

import org.scalatest._

import scalax.file.Path
import scalax.io.Codec

class Md5RecurseTest extends FlatSpec with TestConfig {

  "Md5Recurse find missing files from global" should "print missing file (test both relative and absolute path)" in {
    val testDirPath = copyTestResources
    val file = testDirPath / "dummy1.log"
    val filePathString = file.toAbsolute.path
    val params = Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR)
    val paramsMissing = params :+ "--onlyPrintMissing"

    // Generate global file, then check no missing is printed
    md5Recurse(params :+ testDirPath.path)
    md5RecurseGetOutput(paramsMissing :+ testDirPath.path) should not include("Missing")

    file.delete()

    md5RecurseGetOutput(paramsMissing :+ testDirPath.path, true) should include(s"Missing $filePathString")
    md5RecurseGetOutput(paramsMissing :+ testDirPath.toAbsolute.path, true) should include(s"Missing $filePathString")
  }

  private val MD5_DUMMY1 = "4dfb6df790f3b8b2bf84145c6fb32bac"
  private val MD5_DUMMY2 = "23c1098001b690cd74137cbfe716be06"
  private val NEW_CONTENT_STRING = "New Content"
  private val MD5_NEW_CONTENT = "f45cc89d6028945712f568082080e7c5"

  "Md5Recurse scan single file" should "update file attribute" in {
    md5RecurseFile(SRC_TEST_RES_DIR + "/dummy1.log", Some(MD5_DUMMY1))
    md5RecurseFile(SRC_TEST_RES_DIR + "/dummy1.log", Some(MD5_DUMMY1), Array("--print"))
  }

  "Md5Recurse scan relative dir" should "generate correct global file" in {
    val file = Path(SRC_TEST_RES_DIR) / "."
    md5Recurse(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", file.toAbsolute.path))
    val globalFile = Path(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
    globalFile.lines()(Codec.UTF8).exists(s => s.contains("/.") || s.contains(File.separator+".")) should be(false)
  }

  "Md5Recurse file changed" should "update file attribute" in {
    val filename: String = "test-dummy.log"
    val file: File = new File(filename)

    writeFile(filename, "Hej")
    FileUtil.setAttr(FileUtil.attrView(file), Md5FileInfo.ATTR_MD5RECURSE, "wrongMd5")

    Md5Recurse.main(Array(filename))
    validateAttr(file, "95c383ac224e259a5f5d08b4dab08cff")

    new File(filename).delete()
  }

  "Md5Recurse scan single file without dir" should "not crash" in {
    md5RecurseFile("test-res/files/dummy1.log", None)
  }
// Test fails because of bug I havn't solved yet
  "Md5Recurse scan single file without dir" should "global file should not get multiple entries local should be updated" in {
    val testDirPath = copyTestResources
    val filepath1 = testDirPath / "dummy1.log"
    val filepath2 = testDirPath / "dummy2.log"
    val globalMd5FilePath = testDirPath / Path("_global.md5data")
    globalMd5FilePath.exists should be(false)
    val localMd5FilePath = testDirPath / Path(".md5data")
    localMd5FilePath.exists should be(false)

    val md5ParamsFile1 = Array("-g", testDirPath.path, "--enableLocalMd5Data", filepath1.path)
    val md5ParamsFile2 = Array("-g", testDirPath.path, "--enableLocalMd5Data", filepath2.path)

    // Test content not duplicated
    for (i <- 1 to 3) {
      Md5Recurse.main(md5ParamsFile1)
      globalMd5FilePath.lines().foreach(l => println(l))
      assertFileContainsExactly(localMd5FilePath, MD5_DUMMY1, 1)
      assertFileContainsExactly(globalMd5FilePath, MD5_DUMMY1, 1)
    }

    // Read file 2, check file 1 still present in md5data
    Md5Recurse.main(md5ParamsFile2)
    globalMd5FilePath.lines().foreach(l => println(l))
    assertFileContains(globalMd5FilePath, MD5_DUMMY1)
    assertFileContains(localMd5FilePath, MD5_DUMMY1)
    assertFileContains(globalMd5FilePath, MD5_DUMMY2)
    assertFileContains(localMd5FilePath, MD5_DUMMY2)

    // Change file 1
    filepath1.write(NEW_CONTENT_STRING)

    Md5Recurse.main(md5ParamsFile1)
    globalMd5FilePath.lines().foreach(l => println(l))
    assertFileNotContains(globalMd5FilePath, MD5_DUMMY1)
    assertFileNotContains(localMd5FilePath, MD5_DUMMY1)
    assertFileContains(globalMd5FilePath, MD5_NEW_CONTENT)
    assertFileContains(localMd5FilePath, MD5_NEW_CONTENT)
    assertFileContains(globalMd5FilePath, MD5_DUMMY2)
    assertFileContains(localMd5FilePath, MD5_DUMMY2)
  }

  "Md5Recurse scan with --globaldir" should "not fail" in {
    val filename: String = SRC_TEST_RES_DIR + "/dummy1.log"

    // scan file
    Md5Recurse.main(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, filename, filename))
    Md5Recurse.main(Array("-g", TEST_EXECUTION_GLOBAL_DIR, filename, filename))
  }

  "double --globaldir --globaldir" should "fail" in {
    val filename: String = SRC_TEST_RES_DIR + "/dummy1.log"
    val file: File = new File(filename)

    // scan file
    Md5Recurse.main(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--globaldir", "d:/temp", filename))
  }

  "local file with empty dir" should "delete local files" in {
    val parentTestDir = cleanTestDir
    val dir = new File(parentTestDir, "emptyDir")
    val filename = new File(dir+"/dummy1.log")
    val fileMd5Data = new File(dir + "/.md5data")
    val fileMd5Sum = new File(dir + "/.md5")
    if (!dir.exists()) dir.mkdir()
    writeFile(fileMd5Data,"")
    writeFile(fileMd5Sum,"")
    fileMd5Data.exists() should be (true)
    fileMd5Sum.exists() should be (true)

    Md5Recurse.main(Array("--enableLocalMd5Data", "--enableLocalMd5Sum", dir.getPath))

    fileMd5Data.exists() should be (false)
    fileMd5Sum.exists() should be (false)
  }

}