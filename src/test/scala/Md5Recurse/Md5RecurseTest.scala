package Md5Recurse

import java.io.File

import org.scalatest._
import scalax.file.Path
import scalax.io.Codec

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

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
                        files += Md5FileInfo.parseMd5DataLine("dummydir", line).fileName
                    }
                }
                //        println(line)
            }
        }
        //    dirs.foreach(println)
        assert(isSorted(dirs) === true)
    }

    behavior of "Md5Recurse with global"

    it should "generate correct global file without '>xxx/.' paths in global md5data file" in {
        val testDir = copyTestResources
        val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
        md5Recurse("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path)
        globalFile.lines()(Codec.UTF8).exists(s => s.contains("/.") || s.contains(File.separator + ".")) should be(false)
    }

    it should "create relativized paths in global md5data when using --globaldir-relative" in {
        val testDir = copyTestResources
        val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
        md5Recurse("--globaldir-relative", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path)
        val lines = globalFile.lines()(Codec.UTF8).toList
        println("Global dir file content")
        lines.foreach(println)
        lines.exists(_.contains(">.." + File.separator)) should be(true)
        lines.exists(_.startsWith(">" + testDir.toAbsolute.path)) should be(false)
        lines.exists(l => l.matches("^" + MD5_SIMPLE + ".*" + FILENAME_SIMPLE + "$")) should be(true)
    }

    it should "not allow --globaldir and --globaldir-relativized" in {
        def evalCheckForError(params: Array[String]) {
            val errorString = "Error: Use only one of --globaldir (-g) and --globaldir-relative (-G)"
            val (_, err) = md5RecurseGetOutputAndError(params, true)
            err should include(errorString)
        }

        evalCheckForError(Array(paramGlobalDirRelative, ".", paramGlobalDir, ".", "nonExistentDummy"))
        evalCheckForError(Array(paramGlobalDir, ".", paramGlobalDirRelative, ".", "nonExistentDummy"))
        evalCheckForError(Array(paramGlobalDirRelativeShort, ".", paramGlobalDirShort, ".", "nonExistentDummy"))
        evalCheckForError(Array(paramGlobalDirShort, ".", paramGlobalDirRelativeShort, ".", "nonExistentDummy"))
    }

    it should "skip file-read when lastModified up2date" in {
        val testDir = copyTestResources
        val output = md5RecurseGetOutput(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path))
        assert(ExecutionLog.current.readFileAndGeneratedMd5 == true)
        output should include("New") // Verbose print Check we scanned files

        val output2 = md5RecurseGetOutput(Array("-g", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path))
        assert(ExecutionLog.current.readFileAndGeneratedMd5 == false)
        output2 should include("file read skipped") // Check we scanned files
    }

    it should "print global files sorted by directories" in {
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

    it should "print missing file (test both scan-dir in relative and absolute format)" in {
        val testDirPath = copyTestResources
        val params = Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR)
        val globalFile = TEST_EXECUTION_GLOBAL_FILE
        val paramsMissing = params :+ "--only-print-missing"

        // Generate global file, then check no missing is printed
        md5Recurse(params :+ testDirPath.path)
        md5RecurseGetOutput(paramsMissing :+ testDirPath.path, false) should not include ("Missing:")

        val newFile = testDirPath / "newFile.foobar"
        newFile.write(NEW_CONTENT_STRING)
        val file1 = testDirPath / "dummy1.log"
        val file2 = testDirPath / "simple" / "simple.log"
        val file1PathString = file1.toAbsolute.path
        val file2PathString = file2.toAbsolute.path

        file1.delete()
        file2.delete()

        def assertOutput(output: String): Unit = {
            output should include(s"Missing: $file1PathString")
            output should include(s"Missing: $file2PathString")
        }

        assertOutput(md5RecurseGetOutput(paramsMissing :+ testDirPath.path))
        assertOutput(md5RecurseGetOutput(paramsMissing :+ testDirPath.toAbsolute.path))
        val lines = globalFile.lines().toList
        lines.filter(_.contains(MD5_NEW_CONTENT)).isEmpty should be(true) // we should not have updated md5 global data file
        lines.filter(_.contains(newFile.name)).isEmpty should be(true) // we should not have updated md5 global data file
    }

    it should "print modified file" in {
        val testDirPath = copyTestResources / DIR_SIMPLE
        val params = Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3")
        val globalFile = TEST_EXECUTION_GLOBAL_FILE
        val paramsModified = params :+ paramOnlyPrintModified

        // Generate global file, then check no missing is printed
        md5Recurse(params :+ testDirPath.path)
        md5RecurseGetOutput(paramsModified :+ testDirPath.path, true) should not include ("Modified") // Here we have not modified files yet

        val file2 = testDirPath / FILENAME_SIMPLE
        val filePathString = file2.toAbsolute.path
        file2.write(NEW_CONTENT_STRING)

        def assertOutput(output: String): Unit = {
            val outputLines: Array[String] = output.split(Sys.lineSeparator)
            // all three lines below are pretty equal
//            output should include("Modified: " + filePathString)
            outputLines.exists(_ == "Modified: " + filePathString) should be(true)
//            outputLines.exists(_.matches("^Modified:.*" + file2.name + "$")) should be(true)
        }

        println("-------- AFTER Modifing files ---- doing " + paramOnlyPrintModified)
        // file attributes have been disabled to avoid first call updating lastModified timestamp
        assertOutput(md5RecurseGetOutput(paramsModified :+ testDirPath.path))
        assertOutput(md5RecurseGetOutput(paramsModified :+ testDirPath.toAbsolute.path))

        // Check md5data file was not updated, and then run a normal scan and verify it was updated
        println("-------- AFTER Test - doing regular scan")
        globalFile.lines().filter(_.contains(MD5_NEW_CONTENT)).isEmpty should be(true) // we should not have updated md5 global data file
        globalFile.lines().foreach(println)
        md5Recurse(params :+ testDirPath.path)
        globalFile.lines().filter(_.contains(MD5_NEW_CONTENT)).isEmpty should be(false) // now we should se changed file
    }

    it should "parse correctly with slashes on windows" in {
        val testDirPath = copyTestResources / "simple"
        val params = Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--disable-file-attributes", "-V", "3", testDirPath.path)

        println("--------------- Md5Recurse initial run:")
        md5Recurse(params) // Generate initial
        println()

        val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
        // Read entire file
        val globalFileContent = globalFile.string(Codec.UTF8)
        println("--------------- Unmodified global file:")
        println(globalFileContent)
        val globalFileContentWithSlashes = globalFileContent.replace("\\", "/")
        println("--------------- Hacked global file:")
        println(globalFileContentWithSlashes)
        globalFile.write(globalFileContentWithSlashes)(Codec.UTF8)

        println("--------------- Md5Recurse on modified global file with slashes:")
        md5RecurseGetOutput(params, true) should not include ("New")

        val globalFileContentWithSlashesWithoutDrive = globalFileContentWithSlashes.replaceAll(">.:/", ">/")
        println("--------------- Hacked global file2:")
        println(globalFileContentWithSlashesWithoutDrive)
        globalFile.write(globalFileContentWithSlashesWithoutDrive)(Codec.UTF8)

        println("--------------- Md5Recurse on modified global file with slashes or drive:")
        md5RecurseGetOutput(params, true) should not include ("New")
    }

    it should "fail when with double --globaldir --globaldir" in {
        val (_, err) = md5RecurseGetOutputAndError(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--globaldir", "d:/temp", "foobar"))
        err should include("Unknown option --globaldir")
    }

    behavior of "Md5Recurse with --print-modified"

    it should "print modified filenames with globaldir" in {
        val testDir = copyTestResources / "simple"
        val params = Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--print-modified", testDir.path)
        val file = testDir / "simple.log"
        val filePathString = file.toAbsolute.path

        md5Recurse(params)
        (testDir / "simple.log").write("new content")
        md5RecurseGetOutput(params, true) should include("Modified: " + filePathString)
        md5RecurseGetOutput(params, true) should not include ("Modified: " + filePathString)
    }

    //    "Md5Recurse find missing files without global dir" should "fail" in {
    it should "fail without globaldir" in {
        val (_, stdErr) = md5RecurseGetOutputAndError(Array("--only-print-missing", TEST_EXECUTION_DIR), false)
        stdErr should include("Error: --only-print-missing requires --globaldir")
    }

    it should "warn user local dir is unused with local dir" in {
        val testDirPath = copyTestResources
        println(testDirPath.path)
        val (stdOut, _) = md5RecurseGetOutputAndError(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--only-print-missing", "--local", testDirPath.path), true)
        stdOut should include("WARNING: local dir files will not be read when searching for missing files")
    }

    def runMd5AndCheckGeneratedAndDeletedLocalFiles(testDirPath: Path, md5dataFilename: String, params: Array[String]): Unit = {
        val subDir = testDirPath / "sub" / "lastSub"

        println("--------------- Md5Recurse initial run:")
        md5Recurse(params) // Generate initial
        println()

        val localFileWhichShouldBeDeleted = testDirPath / md5dataFilename
        localFileWhichShouldBeDeleted.exists should be(true)
        val localFileWhichShouldBeDeleted2 = subDir / md5dataFilename
        localFileWhichShouldBeDeleted2.exists should be(true)

        val disableMd5Path = testDirPath / ".disable_md5"
        disableMd5Path.write("")
        disableMd5Path.exists should be(true)

        md5Recurse(params) // Generate initial

        localFileWhichShouldBeDeleted.exists should be(false)
        localFileWhichShouldBeDeleted2.exists should be(false)
    }

    "Md5Recurse disabled md5 with local data in dir" should "delete old local md5sum file" in {
        val testDirPath = copyTestResources
        runMd5AndCheckGeneratedAndDeletedLocalFiles(testDirPath, ".user.md5", Array("--local", "-p", "user", "-V", "2", testDirPath.path))
    }

    "Md5Recurse scan single file" should "update file attribute" in {
        md5RecurseFile(SRC_TEST_RES_FILES_DIR + "/dummy1.log", Some(MD5_DUMMY1))
        md5RecurseFile(SRC_TEST_RES_FILES_DIR + "/dummy1.log", Some(MD5_DUMMY1), Array("--print"))
    }

    "Md5Recurse quiet and verbose" should "warn user" in {
        val testDir = copyTestResources / "simple"
        Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
        val output = md5RecurseGetOutput(Array("-q", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.path))
        output should include("Quiet and verbose specified at the same time")
    }

    "Md5Recurse scan" should "work with filesnames with {}()" in {
        val testDir = copyTestResources
        val testFile = testDir / "mytest({})"
        getAttr(testFile) should be(None)
        md5Recurse(testFile.path)
        assert(ExecutionLog.current.readFileAndGeneratedMd5 == true)
        getAttr(testFile).get should include("541c57960bb997942655d14e3b9607f9")
    }

    "Md5Recurse .disabled_md5" should "not scan subdir" in {
        val testdir = Path.fromString(SRC_TEST_RES_FILES_DIR) / "disabled_md5"
        assert(testdir.exists === true)
        val globalFile = Path.fromString(TEST_EXECUTION_GLOBAL_DIR) / "_global.md5data"
        globalFile.delete(true)
        assert(globalFile.exists === false)
        val out = md5RecurseGetOutput(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, testdir.path))
        out should include(".disable_md5 found")
        globalFile.lines().toList.size should be(0)
        println("-----*")
        // silent scan should not echo info
        val out2 = md5RecurseGetOutput(Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-q", testdir.path))
        out2 should not include (".disable_md5 found")
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

    "With global and local enabled" should "not duplicate content" in {
        val testDirPath = copyTestResources
        val filepath1 = testDirPath / "dummy1.log"
        val globalMd5FilePath = testDirPath / Path("_global" + MD5DATA_EXT)
        globalMd5FilePath.exists should be(false)
        val localMd5FilePath = testDirPath / Path(MD5SUM_EXT)
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

    "With global and local enabled and global up2date" should "not read local file" in {
        val testDirPath = copyTestResources / "simple"
        val localMd5FilePath = testDirPath / Path(MD5SUM_EXT)

        // First run once to generate localMd5File (this is not the test)
        md5Recurse("-g", TEST_EXECUTION_GLOBAL_DIR, "--local", testDirPath.path)

        // Now remove the comment from the .md5 line, and check that the missing comment is not detected, meaning we did not read the local file
        def destroyLocalFileContentSoMd5RecurseShouldComplain(): Unit = {
            localMd5FilePath.exists should be(true)
            val lines = localMd5FilePath.lines().toList
            val content = lines.tail.mkString
            localMd5FilePath.write(content)
        }

        def runAndCheckMissingComment(params: Array[String], expectWarning: Boolean): Unit = {
            val (_, error) = md5RecurseGetOutputAndError(params)
            error.contains("Missing md5data") should be(expectWarning)
        }

        // The run md5Recurse twice, and check it warns when it should read local-md5 file, and then check it doesn't warn when it shouldn't read file.
        // The latter indicates it didn't read the file, which is what we want to test here
        destroyLocalFileContentSoMd5RecurseShouldComplain()
        runAndCheckMissingComment(Array("-g", TEST_EXECUTION_GLOBAL_DIR, "--local", testDirPath.path), expectWarning = false)

        destroyLocalFileContentSoMd5RecurseShouldComplain() // Above writes local files
        // Now check error does contain warning (testing that the test idea still works)
        runAndCheckMissingComment(Array("--local", testDirPath.path), expectWarning = true)
    }

    "Md5Recurse scan single file without dir" should "global and local should be updated" in {
        val testDirPath = copyTestResources
        val filepath1 = testDirPath / "dummy1.log"
        val filepath2 = testDirPath / "dummy2.log"
        val globalMd5FilePath = testDirPath / Path("_global" + MD5DATA_EXT)
        globalMd5FilePath.exists should be(false)
        val localMd5FilePath = testDirPath / Path(MD5SUM_EXT)
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

    "local file with empty dir" should "delete local files" in {
        val parentTestDir = cleanTestDir
        val dir = new File(parentTestDir, "emptyDir")
        val fileMd5Sum = new File(dir + "/" + MD5SUM_EXT)
        if (!dir.exists()) dir.mkdir()
        writeFile(fileMd5Sum, "")
        fileMd5Sum.exists() should be(true)

        Md5Recurse.main(Array("--local", dir.getPath))

        // After last md5Recurse execution right above
        Path.fromString(dir.getPath).copyTo(TEST_EXECUTION_ARCHIVE_PATH / "local file with empty dir should delete local files")

        fileMd5Sum.exists() should be(false)
       }

    "with --log and no encoding" should "print to log with utf-8" in {
        val testDir = copyTestResources
        val logFile = s"$TEST_EXECUTION_GLOBAL_DIR/out.log"
        val fileWithNonAsciiChars = testDir / "Utf-8-æøåÆØÅ-\u4e16\u754c\u4f60\u597d\uff01.log"
        fileWithNonAsciiChars.write("some content to create file")

        new File(logFile).exists() should be(false) // pre-test

        // Default encoding should be UTF-8
        md5Recurse("--log", logFile, "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", testDir.toAbsolute.path)

        testDir.copyTo(TEST_EXECUTION_ARCHIVE_PATH / "with --log and no encoding should print to log with utf-8")

        new File(logFile).exists() should be(true)
        val logLines = Path.fromString(logFile).lines()(codec = Codec.UTF8)
        logLines.mkString("\n").contains("Utf-8-æøåÆØÅ-\u4e16\u754c\u4f60\u597d\uff01.log") should be(true)
    }

    "with --log and iso encoding" should "print to log with iso-8859-1" in {
        val testDir = copyTestResources
        val logFile = s"$TEST_EXECUTION_GLOBAL_DIR/out.log"
        val fileWithNonAsciiChars = testDir / "ISO-8859-1-æøåÆØÅ.log"
        fileWithNonAsciiChars.write("some content to create file")

        new File(logFile).exists() should be(false) // pre-test

        // Default encoding should be UTF-8
        md5Recurse("--log", logFile, "--globaldir", TEST_EXECUTION_GLOBAL_DIR, "--encoding", "iso-8859-1", "-V", "3", testDir.toAbsolute.path)

        testDir.copyTo(TEST_EXECUTION_ARCHIVE_PATH / "with --log and iso encoding should print to log with iso-8859-1")

        new File(logFile).exists() should be(true)
        val logLines = Path.fromString(logFile).lines()(codec = Codec.ISO8859)
        logLines.mkString("\n").contains("ISO-8859-1-æøåÆØÅ.log") should be(true)
    }

    "Md5Recurse simple no-arg" should "print error" in {
        md5RecurseGetError(Array()) should include("Error: Missing argument")
    }

    behavior of "Non-tests"

    ignore should "print help - a non-test" in {
        // my getOutput does not catch it
        md5Recurse(Array("--help"))
    }

}