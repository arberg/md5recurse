package Md5Recurse

import better.files
import org.scalatest.{FreeSpec, Matchers}
import scalax.file.Path


class Md5RecurseCheckTest extends FreeSpec with TestConfig with TestData with Matchers {

    def fixtureSimple(globalFileInTestDir: Boolean = false) =
        new {
            val testDir = copyTestResources / DIR_SIMPLE
            val globalDir = if (globalFileInTestDir) testDir else TEST_EXECUTION_GLOBAL_FILE
            val globalFile = globalDir / GLOBAL_BASE_FILENAME
            val file = testDir / FILENAME_SIMPLE
            val newFile = testDir / "newFile.txt"
        }

    def fixture2(globalFileInTestDir: Boolean = false) =
        new {
            val testDir = copyTestResources / DIR_ONLY_TWO_FILES
            val globalDir = if (globalFileInTestDir) testDir else TEST_EXECUTION_GLOBAL_FILE
            val globalFile = globalDir / GLOBAL_BASE_FILENAME
            val file1 = testDir / FILENAME_ONLY_TWO_FILES_1
            val file2 = testDir / FILENAME_ONLY_TWO_FILES_2
        }

    "Md5Recurse with --check-all" - {
        "using --globaldir" - {
            "should detect changes in files with modified timestamps" in {
                val f = fixture2()
                val params = Array("--globaldir", TEST_EXECUTION_GLOBAL_DIR, "-V", "3", f.testDir.toAbsolute.path)
                md5Recurse(params)
                val orgLastModified = f.file1.lastModified
                f.file1.write("new content")
                assert(orgLastModified != f.file1.lastModified) // file timestamp should be changed for test to work
                val (_, err) = md5RecurseGetOutputAndError(params :+ paramCheckAll, true)
                err should include("Failed verification but with modified timestamp: original=681cde2a4b71e4881e3982220c3514d4 current=96c15c2bb2921193bf290df8cd85e2ba")
            }
        }
    }

    "Md5Recurse with --check" - {
        def checkScanAndCheckNewFileChangesAreScanned(globalDir: Path, globalFile: Path, testDir: Path, fileToChange: Path) {
            md5Recurse(paramDisableFileAttributes, paramGlobalDir, globalDir.path, "-V", "3", testDir.toAbsolute.path);
            fileToChange.write(NEW_CONTENT_STRING)

            val out = md5RecurseGetOutput(Array(paramDisableFileAttributes, paramGlobalDir, globalDir.path, "-V", "3", paramCheck, testDir.toAbsolute.path))
            out should include(fileToChange.name)
            out should include(MD5_NEW_CONTENT)
            globalFile.lines().exists(line => line.contains(MD5_NEW_CONTENT)) should be(true)
        }
        "using --globaldir" - {
            "should detect changes for identical lastModified timestamp" in {
                val f = fixtureSimple(true)
                md5Recurse(paramDisableFileAttributes, paramGlobalDir, f.testDir.path, "-V", "3", f.testDir.toAbsolute.path);
                replaceMd5LineInFile(f.globalFile, "simple.log", "a" * 32)

                val (_, err) = md5RecurseGetOutputAndError(Array(paramDisableFileAttributes, paramGlobalDir, f.testDir.path, "-V", "3", paramCheck, f.testDir.toAbsolute.path))
                val failVerificationMessage = "Failed verification: original=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa current=c4ca4238a0b923820dcc509a6f75849b"
                err should include(failVerificationMessage)
                val logFile: files.File = better.files.File(f.testDir.path).list.filter(f => f.name.endsWith("_global_failed.log")).toList.head
                logFile.lines().exists(_.contains(failVerificationMessage)) should be(true)
            }

            "should scan files with changed timestamp" in {
                val f = fixtureSimple(true)
                checkScanAndCheckNewFileChangesAreScanned(f.globalDir, f.globalFile, f.testDir, f.file)
            }

            "should scan new files" in {
                val f = fixtureSimple(true)
                checkScanAndCheckNewFileChangesAreScanned(f.globalDir, f.globalFile, f.testDir, f.newFile)
            }
        }

        "using --globaldir-relative" - {
            "should detect changes for identical lastModified timestamp - when using global file in scanned dir root" in {
                val testDir: Path = copyTestResources / "simple"
                val globalFile = testDir / "_global.md5data"
                md5Recurse(paramDisableFileAttributes, paramGlobalDirRelative, testDir.path, "-V", "3", testDir.toAbsolute.path);
                replaceMd5LineInFile(globalFile, "simple.log", "a" * 32)

                val failVerificationMessage = "Failed verification: original=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa current=c4ca4238a0b923820dcc509a6f75849b"
                md5RecurseGetError(Array(paramDisableFileAttributes, paramGlobalDirRelative, testDir.path, "-V", "3", "--check", testDir.toAbsolute.path)) should include(failVerificationMessage)
                val logFile: files.File = better.files.File(testDir.path).list.filter(f => f.name.endsWith("_global_failed.log")).toList.head
                logFile.lines().exists(_.contains(failVerificationMessage)) should be(true)
            }

            "should scan files with changed timestamp" in {
                val f = fixtureSimple(true)
                checkScanAndCheckNewFileChangesAreScanned(f.globalDir, f.globalFile, f.testDir, f.file)
            }

            "should scan new files" in {
                val f = fixtureSimple(true)
                checkScanAndCheckNewFileChangesAreScanned(f.globalDir, f.globalFile, f.testDir, f.newFile)
            }
        }
    }

    "Md5Recurse with --check-only" - {
        def checkOnlyScanAndCheckNewFileChangesAreNotScanned(globalDir: Path, globalFile: Path, testDir: Path, fileToChange: Path) {
            md5Recurse(paramDisableFileAttributes, paramGlobalDir, globalDir.path, "-V", "3", testDir.toAbsolute.path);
            fileToChange.write(NEW_CONTENT_STRING)

            val out = md5RecurseGetOutput(Array(paramDisableFileAttributes, paramGlobalDir, globalDir.path, "-V", "3", paramCheckOnly, testDir.toAbsolute.path))
            out should not include(fileToChange.name)
            out should not include(MD5_NEW_CONTENT)
            globalFile.lines().exists(line => line.contains(MD5_NEW_CONTENT)) should not be(true)
        }

        "using --globaldir" - {
            "should detect changes for identical lastModified timestamp" in {
                val f = fixtureSimple(true)
                md5Recurse(paramDisableFileAttributes, paramGlobalDir, f.testDir.path, "-V", "3", f.testDir.toAbsolute.path);
                replaceMd5LineInFile(f.globalFile, "simple.log", "a" * 32)

                val failVerificationMessage = "Failed verification: original=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa current=c4ca4238a0b923820dcc509a6f75849b"
                md5RecurseGetError(Array(paramDisableFileAttributes, paramGlobalDir, f.testDir.path, "-V", "3", paramCheckOnly, f.testDir.toAbsolute.path)) should include(failVerificationMessage)
                val logFile: files.File = better.files.File(f.testDir.path).list.filter(f => f.name.endsWith("_global_failed.log")).toList.head
                logFile.lines().exists(_.contains(failVerificationMessage)) should be(true)
            }


            "should not scan files with changed timestamp" in {
                val f = fixtureSimple(true)
                checkOnlyScanAndCheckNewFileChangesAreNotScanned(f.globalDir, f.globalFile, f.testDir, f.file)
            }

            "should not scan new files" in {
                val f = fixtureSimple(true)
                checkOnlyScanAndCheckNewFileChangesAreNotScanned(f.globalDir, f.globalFile, f.testDir, f.newFile)
            }
        }
    }
}