package Md5Recurse

import java.io.File

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scalax.file.Path
import scalax.io.Codec

@RunWith(classOf[JUnitRunner])
class Md5RecurseTest extends FlatSpec with TestConfig with TestData {

  class CommonInvoker(commonParams: Array[String]) {
    def doMd5RecursePath(path: Path): Unit = {
      doMd5RecursePath(path.path)
    }

    def doMd5RecursePath(path: String): Unit = {
      Md5Recurse.main(commonParams ++ Array(path))
    }
  }

  def isSorted(l: ListBuffer[String]): Boolean = l == l.sorted

  def verifyMd5DataSorting(md5DataPath: Path) = {
    val dirs = new ListBuffer[String]
    val files = new ListBuffer[String]
    md5DataPath lines() foreach {
      line => {
        if (line.length > 0) {
          if (line.charAt(0) == '>') {
            dirs += line.substring(0)
            assert(isSorted(files) === true, files)
            files.clear()
          } else {
            files += Md5FileInfo.parseMd5DataLine("dummydir", line).fileName()
          }
        }
        //        println(line)
      }
    }
    //    dirs.foreach(println)
    assert(isSorted(dirs) === true)
  }


  "Md5Recurse sorting" should "print global files sorted by directories" in {
    val testDirPath = copyTestResources

    val aToZ: immutable.Seq[Char] = 'a' to 'z'
    val shuffled = Random.shuffle(aToZ)
    for (c <- shuffled) {
      val newDirectory = testDirPath / c.toString
      newDirectory.createDirectory()
      for (c <- shuffled) {
        val newFile = newDirectory / c.toString
        newFile.createFile()
        newFile.write(c.toString)
        //        println(newFile.path)
      }
      //      println(newDirectory.path)
    }
    new CommonInvoker(Array("-g", TEST_EXECUTION_GLOBAL_DIR, "-p", "sorting")) {
      // verify sorting for newly generated global file
      doMd5RecursePath(testDirPath)
      verifyMd5DataSorting(TEST_EXECUTION_GLOBAL_DIR_PATH / "sorting_global.md5data")
      // Verify sorting when global files exists
      doMd5RecursePath(testDirPath)
      verifyMd5DataSorting(TEST_EXECUTION_GLOBAL_DIR_PATH / "sorting_global.md5data")
      // verify sorting for global file which is mostly printed from printOutSideScope
      // First line will currently be unsorted
      //      doMd5RecursePath(testDirPath / "a")
      //      verifyMd5DataSorting(TEST_EXECUTION_GLOBAL_DIR_PATH / "sorting_global.md5data")
    }

  }

  "Md5Recurse find missing files from global" should "print missing file (test both relative and absolute path)" in {
    val testDirPath = copyTestResources
    val params = Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR)
    val paramsMissing = params :+ "--print-missing"

    // Generate global file, then check no missing is printed
    md5Recurse(params :+ testDirPath.path)
    md5RecurseGetOutput(paramsMissing :+ testDirPath.path, false) should not include ("Missing")

    val file1 = testDirPath / "dummy1.log"
    val file2 = testDirPath / "simple" / "simple.log"
    val file1PathString = file1.toAbsolute.path
    val file2PathString = file2.toAbsolute.path

    file1.delete()
    file2.delete()

    def assertOutput(output: String): Unit = {
      output should include(s"Missing $file1PathString")
      output should include(s"Missing $file2PathString")
    }

    assertOutput(md5RecurseGetOutput(paramsMissing :+ testDirPath.path))
    assertOutput(md5RecurseGetOutput(paramsMissing :+ testDirPath.toAbsolute.path))
  }

  "Md5Recurse find missing files without global dir" should "fail" in {
    val (_, stdErr) = md5RecurseGetOutputAndError(Array("--print-missing", TEST_EXECUTION_DIR), false)
    stdErr should include("Error: --print-missing requires --globaldir")
  }

  "Md5Recurse find missing files with local dir" should "warn user local dir is unused" in {
    val testDirPath = copyTestResources
    println(testDirPath.path)
    val (stdOut, _) = md5RecurseGetOutputAndError(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--print-missing", "--local", testDirPath.path), true)
    stdOut should include("WARNING: local dir files will not be read when searching for missing files")
  }

  "Md5Recurse scan single file" should "update file attribute" in {
    md5RecurseFile(SRC_TEST_RES_FILES_DIR + "/dummy1.log", Some(MD5_DUMMY1))
    md5RecurseFile(SRC_TEST_RES_FILES_DIR + "/dummy1.log", Some(MD5_DUMMY1), Array("--print"))
  }

  "Md5Recurse scan relative dir" should "generate correct global file" in {
    val testDir = copyTestResources
    val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
    val output = md5RecurseGetOutput(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path))
    assert(ExecutionLog.current.readFileAndGeneratedMd5 == true)
    output should include("New") // Verbose print Check we scanned files
    globalFile.lines()(Codec.UTF8).exists(s => s.contains("/.") || s.contains(File.separator + ".")) should be(false)

    val output2 = md5RecurseGetOutput(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path))
    assert(ExecutionLog.current.readFileAndGeneratedMd5 == false)
    output2 should include("file read skipped") // Check we scanned files
  }

  "Md5Recurse check-all" should "detect changes in files with modified timestamps" in {
    val verboseLevel = "3"
    val testDir = copyTestResources / "onlyTwoFiles"
    val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
    val params = Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", verboseLevel, testDir.toAbsolute.path)
    md5Recurse(params)
    val file = testDir / "dummy1a.log"
    val orgLastModified = file.lastModified
    file.write("new content")
    assert(orgLastModified != file.lastModified) // file timestamp should be changed for test to work
    val (_, err) = md5RecurseGetOutputAndError(params :+ "--check-all", true)
    err should include("Failed verification but with modified timestamp: original=681cde2a4b71e4881e3982220c3514d4 current=96c15c2bb2921193bf290df8cd85e2ba")
  }

  "Md5Recurse quiet and verbose" should "warn user" in {
    val testDir = copyTestResources / "simple"
    Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
    val output = md5RecurseGetOutput(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.path))
    output should include("Quiet and verbose specified at the same time")
  }

  "Md5Recurse --print-modified" should "print modified filenames" in {
    val testDir = copyTestResources / "simple"
    val params = Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--print-modified", testDir.path)
    md5Recurse(params)
    (testDir / "dummy1a.log").write("new content")
    val output = md5RecurseGetOutput(params, true)

    val file = testDir / "simple.log"
    val filePathString = file.toAbsolute.path
    output should include("Modified " + filePathString)
  }

  "Md5Recurse scan" should "work with filesnames with {}()" in {
    val testDir = copyTestResources
    val testFile = testDir / "mytest({})"
    getAttr(testFile) should be(None)
    md5Recurse(testFile.path)
    assert(ExecutionLog.current.readFileAndGeneratedMd5 == true)
    getAttr(testFile).get should include("541c57960bb997942655d14e3b9607f9")
  }

  "Md5Recurse .disabled_md5" should "not scane subdirs" in {
    val testdir = Path.fromString(SRC_TEST_RES_FILES_DIR) / "disabled_md5"
    assert(testdir.exists === true)
    val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
    globalFile.delete(true)
    assert(globalFile.exists === false)
    md5Recurse(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testdir.path))
    globalFile.lines().toList.size should be(0)
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
    md5RecurseFile(SRC_TEST_RES_FILES_DIR + "/dummy1.log", None)
  }

  "Md5Recurse scan" should "global and local files not duplicated content" in {
    val testDirPath = copyTestResources
    val filepath1 = testDirPath / "dummy1.log"
    val globalMd5FilePath = testDirPath / Path("_global.md5data")
    globalMd5FilePath.exists should be(false)
    val localMd5FilePath = testDirPath / Path(".md5data")
    localMd5FilePath.exists should be(false)

    val commonParams = Array("-g", testDirPath.path, "--local")
    val md5ParamsFile1 = commonParams ++ Array(filepath1.path)

    // Test content not duplicated
    Md5Recurse.main(md5ParamsFile1)
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    Md5Recurse.main(md5ParamsFile1)
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    // Test file relative path upper/lower case
    Md5Recurse.main(commonParams ++ Array(filepath1.path.toLowerCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    Md5Recurse.main(commonParams ++ Array(filepath1.path.toUpperCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    // Test file absolute path upper/lower case
    Md5Recurse.main(commonParams ++ Array(filepath1.toAbsolute.path.toLowerCase))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    Md5Recurse.main(commonParams ++ Array(filepath1.toAbsolute.path.toUpperCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    // Test dirs
    Md5Recurse.main(commonParams ++ Array(testDirPath.path))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath) // Here we get duplication if we don't use getCanonicalFile
    // Test dirs relative path upper/lower case
    Md5Recurse.main(commonParams ++ Array(testDirPath.path.toUpperCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath) // Here we get duplication if we don't use getCanonicalFile
    Md5Recurse.main(commonParams ++ Array(testDirPath.path.toLowerCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
    // Test dirs absolute path upper/lower case
    Md5Recurse.main(commonParams ++ Array(testDirPath.toAbsolute.path.toUpperCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath) // Here we get duplication if we don't use getCanonicalFile
    Md5Recurse.main(commonParams ++ Array(testDirPath.toAbsolute.path.toLowerCase()))
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)
  }

  // Test fails because of bug I havn't solved yet
  "Md5Recurse scan single file without dir" should "global and local should be updated" in {
    val testDirPath = copyTestResources
    val filepath1 = testDirPath / "dummy1.log"
    val filepath2 = testDirPath / "dummy2.log"
    val globalMd5FilePath = testDirPath / Path("_global.md5data")
    globalMd5FilePath.exists should be(false)
    val localMd5FilePath = testDirPath / Path(".md5data")
    localMd5FilePath.exists should be(false)

    val commonParams = Array("-g", testDirPath.path, "--local")
    val md5ParamsFile1 = commonParams ++ Array(filepath1.path)
    val md5ParamsFile2 = commonParams ++ Array(filepath2.path)

    Md5Recurse.main(md5ParamsFile1)
    assertFilesContainExactly(MD5_DUMMY1, 1, globalMd5FilePath, localMd5FilePath)

    // Read file 2, check file 1 still present in md5data
    Md5Recurse.main(md5ParamsFile2)
    globalMd5FilePath.lines().foreach(l => println(l))
    assertFilesContain(MD5_DUMMY1, globalMd5FilePath, localMd5FilePath)
    assertFilesContain(MD5_DUMMY2, globalMd5FilePath, localMd5FilePath)

    // Change file 1
    filepath1.write(NEW_CONTENT_STRING)

    Md5Recurse.main(md5ParamsFile1)
    globalMd5FilePath.lines().foreach(l => println(l))
    assertFileNotContains(MD5_DUMMY1, globalMd5FilePath, localMd5FilePath)
    assertFilesContain(MD5_NEW_CONTENT, globalMd5FilePath, localMd5FilePath)
    assertFilesContain(MD5_DUMMY2, globalMd5FilePath, localMd5FilePath)
  }

  "Md5Recurse scan with --globaldir" should "not fail" in {
    val filename: String = SRC_TEST_RES_FILES_DIR + "/dummy1.log"

    // scan file
    Md5Recurse.main(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, filename, filename))
    Md5Recurse.main(Array("-g", TEST_EXECUTION_GLOBAL_DIR, filename, filename))
  }

  "double --globaldir --globaldir" should "fail" in {
    val filename: String = SRC_TEST_RES_FILES_DIR + "/dummy1.log"
    val file: File = new File(filename)

    // scan file
    Md5Recurse.main(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--globaldir", "d:/temp", filename))
  }

  "local file with empty dir" should "delete local files" in {
    val parentTestDir = cleanTestDir
    val dir = new File(parentTestDir, "emptyDir")
    val filename = new File(dir + "/dummy1.log")
    val fileMd5Data = new File(dir + "/.md5data")
    val fileMd5Sum = new File(dir + "/.md5")
    if (!dir.exists()) dir.mkdir()
    writeFile(fileMd5Data, "")
    writeFile(fileMd5Sum, "")
    fileMd5Data.exists() should be(true)
    fileMd5Sum.exists() should be(true)

    Md5Recurse.main(Array("--local", "--local-md5sum", dir.getPath))

    fileMd5Data.exists() should be(false)
    fileMd5Sum.exists() should be(false)
  }

}