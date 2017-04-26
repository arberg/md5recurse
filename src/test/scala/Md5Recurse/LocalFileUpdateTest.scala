package Md5Recurse

import java.io.File

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scalax.file.Path
import scalax.io.Codec
// http://jesseeichar.github.io/scala-io-doc/0.4.3/index.html#!/file/string_to_file

@RunWith(classOf[JUnitRunner])
class LocalFileUpdateTest extends FlatSpec with TestConfig with TestData {

  val MD5DATA_EXT = ".md5data"
  val MD5SUM_EXT = ".md5"

  "Timer" should "performance files" in {
    if (false) {
      val testDirPath = copyTestResources / "onlyTwoFiles"
      val file = new File(testDirPath.path)

      val totalLoops = 10000000
      val innerLoops = 10000
      val loops = totalLoops / innerLoops

      def doPeformanceRun(task: () => Unit, name: String): Unit = {
        // warm up
        for (i <- 1 to 100) {
          task.apply()
        }
        val timer = new Timer()
        for (i <- 1 to loops) {
          task.apply()
        }
        val milli = timer.elapsedMilli
        println(name + " time passed " + milli + "ms")
      }

      doPeformanceRun(() => for (i <- 1 to innerLoops) "oseth", "string")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getPath, "getPath")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getAbsolutePath, "AbsolutePath")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getCanonicalPath, "CanonicalPath")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getCanonicalFile, "CanonicalFile")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getCanonicalFile.getCanonicalPath, "CanonicalFile.CanonicalPath")
      doPeformanceRun(() => for (i <- 1 to innerLoops) file.getCanonicalFile.getCanonicalFile.getCanonicalFile.getCanonicalPath, "CanonicalFile.CanonicalPath")
    }
  }

  "Local file" should "only be updated if changes exists" in {
    def testLocalFileWrittenAndNotUpdated(localMd5FileExtension: String, md5ToolParam: Array[String]) {
      println(s"Working on extension: $localMd5FileExtension")
      val testDirPath = copyTestResources / "onlyTwoFiles"
      testDirPath.children().size should be(2)
      val filepath1 = testDirPath / "dummy1a.log"
      val filepath2 = testDirPath / "testfileРС_WithNonISO8859-chars_in_name_Looking_like_PC.log"
      filepath1.exists should be(true)
      filepath2.exists should be(true)

      val prefix = "testLocalFiles"
      val localMd5FilePath = testDirPath / Path("." + prefix + localMd5FileExtension)
      localMd5FilePath.exists should be(false)

      def runMd5Recurse(): Unit = {
        Md5Recurse.main(md5ToolParam ++ Array("-V", "2", "-p", prefix, testDirPath.path)) // todo test without alwaysUpdateLocal
      }

      // Scan directory and test that md5-file is created (md5data or md5sum, see param)
      runMd5Recurse()
      localMd5FilePath.exists should be(true)
      assertFilesContainExactly(MD5_DUMMY1a, 1, localMd5FilePath)

      // Change the test-file and check new md5 file written
      filepath1.write(NEW_CONTENT_STRING)
      runMd5Recurse()
      localMd5FilePath.lines().foreach(println(_))
      assertFilesContainExactlyOnce(MD5_NEW_CONTENT, localMd5FilePath)

      // Run again, but this time we expect that the local files are up2date so they should not be rewritten
      Thread.sleep(if (Sys.isWin) 100 else 1000)
      // OS may have minimum time measurement of 16 ms, so sleep a bit, linux only has lastModified accuracy of 1s
      val localMd5LastModified = localMd5FilePath.lastModified
      runMd5Recurse()
      assert(localMd5FilePath.lastModified === localMd5LastModified, s"file $localMd5FileExtension should not have been updated, because content is the same")

      // Run again, file still up2date, but this time with no attributes written. Note if this is Md5Sum test then this test only has file attribute as data, and since we just delete the file attributes data
      // we expect a rescan to occur for md5sum test. A rescan forces updates of md5sum+md5data so they get new lastModified timestamps and subsequent scans don't read/write them.
      deleteMd5FileAttributes(testDirPath)
      runMd5Recurse()
      //      localMd5FilePath.lines().foreach(println(_))
      println(localMd5FilePath.path)
      if (md5ToolParam.contains("--enableLocalMd5Data"))
        localMd5FilePath.lastModified should be(localMd5LastModified)
      else
        localMd5FilePath.lastModified should not be(localMd5LastModified)

      // Modify the local data file, so lines are not correctly sorted, and run again. File should still be up2date
      val localMd5LinesBeforeReversal = localMd5FilePath.lines().toList
      localMd5FilePath.write("Dummy")
      val localMd5LinesAfterReversal = localMd5FilePath.lines().toList
      filepath1.lastModified = localMd5FilePath.lastModified // update timestamp of file in dir, so we should rescan dir
      runMd5Recurse()
      // Compare actual content instead of timestamps, because then we don't have to sleep to get another lastModified
      localMd5FilePath.lines().foreach(println)
      localMd5FilePath.lines().toList should not be (localMd5LinesAfterReversal)
//      localMd5FilePath.lines().toList should be(localMd5LinesBeforeReversal)

      // Delete the source files and check that md5 file gets cleaned up
      filepath1.delete()
      filepath2.delete()
      runMd5Recurse()
      localMd5FilePath.exists should be(false)
    }

    testLocalFileWrittenAndNotUpdated(MD5SUM_EXT, Array("--enableLocalMd5Sum"))
    testLocalFileWrittenAndNotUpdated(MD5DATA_EXT, Array("--enableLocalMd5Data"))
    testLocalFileWrittenAndNotUpdated(MD5DATA_EXT, Array("--enableLocalMd5Data", "--print"))
    testLocalFileWrittenAndNotUpdated(MD5SUM_EXT, Array("--enableLocalMd5Sum", "--print"))
  }

  "With disabled fileAttributes" should "not read or write fileAttributes" in {
    val testDirPath = copyTestResources
    val filepath = testDirPath / "dummy1.log"
    filepath.exists should be(true)
    val localMd5FilePath = testDirPath / Path(".md5data")
    localMd5FilePath.exists should be(false)

    deleteMd5FileAttributes(testDirPath)
    // Should fail with info 'Error: Please choose storage to read from (--disableReadGlobalMd5 requires --enableLocalMd5Data)'
    //Md5Recurse.main(Array("--disable-file-attributes", testDirPath.path))
    val (_, errorNoStorage) = md5RecurseGetOutputAndError(Array("--disable-file-attributes", filepath.path))
    errorNoStorage should include("Error: Please choose storage to read from")

    setMd5FileAttributes(filepath, "dddddddddddddddddddddddddddddddd 1 1492717460 200")
    Md5Recurse.main(Array("--disable-file-attributes", "--enableLocalMd5Data", filepath.path))
    validateAttr(filepath, "dddddddddddddddddddddddddddddddd") // File attribute should still contain our dummy value
    localMd5FilePath.exists should be(true)
    //localMd5FilePath.lines().foreach(f => println(f))
    val md5DataLine = localMd5FilePath.lines().head
    md5DataLine should include("4dfb6df790f3b8b2bf84145c6fb32bac") // Local file should contain actual computed value
  }

  //  "With enabled fileAttributes" should "read fileAttribute and the attribute should trump local file md5data" in {
  //    val testDirPath = copyTestResources
  //    val filepath = testDirPath / "dummy1.log"
  //    filepath.exists should be(true)
  //
  //    val localMd5FilePath = testDirPath / Path(".md5data")
  //    localMd5FilePath.exists should be(false)
  //    val globalMd5FilePath = testDirPath / Path("_global.md5data")
  //    globalMd5FilePath.exists should be(false)
  //
  //    deleteMd5FileAttributes(testDirPath)
  //    // bug local file and global file date is incorrect when attribute is updated
  //    val md5Params = Array("-g", testDirPath.path, "--enableLocalMd5Data", filepath.path)
  //    println(testDirPath.path)
  //    println(1)
  //    Md5Recurse.main(md5Params)
  //    localMd5FilePath.lines().foreach(l => println(l))
  //    globalMd5FilePath.lines().foreach(l => println(l))
  //    localMd5FilePath.lines().head should include("4dfb6df790f3b8b2bf84145c6fb32bac") // Local file should contain actual computed value
  //    validateAttr(filepath, "4dfb6df790f3b8b2bf84145c6fb32bac") // File attribute should still contain our dummy value
  //    //    validateAttr(filepath, md5DataLine) // File attribute should still contain our dummy value
  //    Thread.sleep(100)
  //  }

  // Test fails because of bug I havn't solved yet
  "With enabled fileAttributes" should "write fileAttribute with correct timestamp so we don't think file modified on next scan" in {
    val testDirPath = copyTestResources
    val filepath = testDirPath / "dummy1.log"
    filepath.exists should be(true)

    def repeatTest(params: Array[String]) = {
      def lastMod() = FileInfoBasic.create(new File(filepath.path)).getLastModified()

      println()
      // If I don't copyTestResources then below won't update file modified timestamp on each run.
      // Ah if just once we manage to run two consequtive scans within same second they get same lastModified and we will stop rescanning, because fileAttribute becomes correct
      copyTestResources
      Md5Recurse.main(params)
      val firstLastModified = lastMod()
      for (i <- 1 to 4) {
        Thread.sleep(20)
        Md5Recurse.main(params)
        val currentLastMod = lastMod()
        if (currentLastMod != firstLastModified) {
          println("ERROR: File changed")
        }
        println(getAttr(filepath).get)
        assert(currentLastMod === firstLastModified)
      }
    }

    repeatTest(Array("-V", "1", filepath.path))
    repeatTest(Array("-g", testDirPath.path, filepath.path))
    repeatTest(Array("-V", "1", "--enableLocalMd5Data", filepath.path))
  }

  "With enabled fileAttributes" should "a renamed file should not be rescanned" in {
    val testDirPath = copyTestResources
    val filepath = testDirPath / "dummy1.log"
    val newFilepath = testDirPath / "dummy1Renamed.log"
    filepath.exists should be(true)

    val commonParams = Array("-V", "2")

    def execute(p: Path) {
      Md5Recurse.main(commonParams ++ Array(p.path))
      println
    }

    execute(filepath)
    ExecutionLog.current.readFileAndGeneratedMd5 should be(true)
    execute(filepath)
    ExecutionLog.current.readFileAndGeneratedMd5 should be(false)
    println(getAttr(filepath) + "\n")
    filepath.moveTo(newFilepath)
    execute(newFilepath)
    ExecutionLog.current.readFileAndGeneratedMd5 should be(false)
  }

  "Local file" should "honor specified encoding" in {
    def testLocalFileWrittenInEncoding(localMd5FileExtension: String, md5ToolParam: Array[String], expectForcedUTF8: Boolean) {
      val testDirPath = copyTestResources / "encoding"
      val filepath = testDirPath / "danish_øæåØÆÅ.log"
      filepath.exists should be(true)

      // scan file
      val localMd5FilePath = testDirPath / localMd5FileExtension
      localMd5FilePath.exists should be(false)

      def verify(testDescription: String, theCodec: Codec) {
        // We expect a local MD5 file has been written
        println(testDescription + ": " + localMd5FileExtension)
        withClue(localMd5FilePath) {
          localMd5FilePath.exists should be(true)
        }
        localMd5FilePath.lines()(codec = theCodec).foreach(println(_))
        localMd5FilePath.lines()(codec = theCodec).exists(_.contains("danish_øæåØÆÅ")) should be(true)
        localMd5FilePath.lines()(Codec.UTF8).exists(_.contains("danish_øæåØÆÅ")) should be(theCodec == Codec.UTF8)
      }

      val commonParams = Array("--alwaysUpdateLocal", testDirPath.path)
      deleteMd5FileAttributes(testDirPath)
      Md5Recurse.main(md5ToolParam ++ commonParams)
      verify("UTF-8 default", Codec.UTF8)
      Md5Recurse.main(md5ToolParam ++ Array("--encoding", "UTF-8") ++ commonParams)
      verify("UTF-8 specified", Codec.UTF8)
      Md5Recurse.main(md5ToolParam ++ Array("--encoding", "ISO-8859-1") ++ commonParams)
      verify("ISO-8859-1 specified", if (expectForcedUTF8) Codec.UTF8 else Codec.ISO8859)
      Md5Recurse.main(md5ToolParam ++ Array("--encoding", "UTF-16") ++ commonParams)
      verify("UTF-16 specified", if (expectForcedUTF8) Codec.UTF8 else Codec("UTF-16"))
    }

    testLocalFileWrittenInEncoding(MD5DATA_EXT, Array("--enableLocalMd5Data"), true)
    testLocalFileWrittenInEncoding(MD5SUM_EXT, Array("--enableLocalMd5Sum"), false)
    testLocalFileWrittenInEncoding(MD5DATA_EXT, Array("--enableLocalMd5Data", "--print"), true)
    testLocalFileWrittenInEncoding(MD5SUM_EXT, Array("--enableLocalMd5Sum", "--print"), false)
  }

  "encoding" should "write .md5 files files with BOM or not BOM" in {
    val testDirPath = copyTestResources / "allfiles"

    // scan file
    val localMd5FilePath = testDirPath / MD5SUM_EXT
    localMd5FilePath.exists should be(false)

    deleteMd5FileAttributes(testDirPath)
    //    Md5Recurse.main(Array("--enableLocalMd5Sum", "--enableLocalMd5Data", "-e", "ISO-8859-1", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, testDirPath.path))
    Md5Recurse.main(Array("--enableLocalMd5Sum", "--enableLocalMd5Data", "-e", "UTF-8-BOM", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, testDirPath.path))
    withClue(localMd5FilePath) {
      localMd5FilePath.exists should be(true)
    }
    localMd5FilePath.lines().filter(_.contains("\uFEFF")).size >= 1 should be(true)

    localMd5FilePath.delete()
    Md5Recurse.main(Array("--enableLocalMd5Sum", "--enableLocalMd5Data", "-e", "UTF-8", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, testDirPath.path))
    localMd5FilePath.exists should be(true)
    localMd5FilePath.lines().foreach(println(_))
    localMd5FilePath.lines().filter(_.contains("\uFEFF")).isEmpty should be(true)

    localMd5FilePath.delete()
    Md5Recurse.main(Array("--enableLocalMd5Sum", "--enableLocalMd5Data", "-e", "ISO-8859-1", "--globaldir", TEST_EXECUTION_GLOBAL_DIR, testDirPath.path))
    localMd5FilePath.exists should be(true)
    localMd5FilePath.lines().foreach(println(_))
    localMd5FilePath.lines().filter(_.contains("\uFEFF")).isEmpty should be(true)

  }

  //  Outcommented because they kill JVM
  //  "help" should "print help" in {
  //    Md5Recurse.main(Array("--help"))
  //  }
  //
  //  "version" should "print version" in {
  //    Md5Recurse.main(Array("--version"))
  //  }

}