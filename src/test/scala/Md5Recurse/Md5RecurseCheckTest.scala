package Md5Recurse

import better.files
import org.scalatest.FreeSpec
import scalax.file.Path


class Md5RecurseCheckTest extends FreeSpec with TestConfig with TestData {

    "Md5Recurse with --check-all" - {
        "using --globaldir" - {
            "should detect changes in files with modified timestamps" in {
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
        }
    }

    "Md5Recurse with --check" - {
        "using --globaldir" - {
            "should detect changes for identical lastModified timestamp" in {
                val testDir: Path = copyTestResources / "simple"
                val globalFile = testDir / "_global.md5data"
                md5Recurse(paramDisableFileAttributes, paramGlobalDir, testDir.path, "-V", "3", testDir.toAbsolute.path);
                replaceMd5LineInFile(globalFile, "simple.log", "a" * 32)

                val (_, err) = md5RecurseGetOutputAndError(Array(paramDisableFileAttributes, paramGlobalDir, testDir.path, "-V", "3", "--check", testDir.toAbsolute.path))
                val failVerificationMessage = "Failed verification: original=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa current=c4ca4238a0b923820dcc509a6f75849b"
                err should include(failVerificationMessage)
                val logFile: files.File = better.files.File(testDir.path).list.filter(f => f.name.endsWith("_global_failed.log")).toList.head
                logFile.lines().exists(_.contains(failVerificationMessage)) should be(true)
            }

        }

        "using --globaldir-relative" - {
            "should detect changes for identical lastModified timestamp - when using global file in scanned dir root" in {
                val testDir: Path = copyTestResources / "simple"
                val globalFile = testDir / "_global.md5data"
                md5Recurse(paramDisableFileAttributes, paramGlobalDirRelative, testDir.path, "-V", "3", testDir.toAbsolute.path);
                replaceMd5LineInFile(globalFile, "simple.log", "a" * 32)

                val (_, err) = md5RecurseGetOutputAndError(Array(paramDisableFileAttributes, paramGlobalDirRelative, testDir.path, "-V", "3", "--check", testDir.toAbsolute.path), true)
                val failVerificationMessage = "Failed verification: original=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa current=c4ca4238a0b923820dcc509a6f75849b"
                err should include(failVerificationMessage)
                val logFile: files.File = better.files.File(testDir.path).list.filter(f => f.name.endsWith("_global_failed.log")).toList.head
                logFile.lines().exists(_.contains(failVerificationMessage)) should be(true)
            }
        }
    }
}