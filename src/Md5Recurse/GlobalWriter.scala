package Md5Recurse

import java.io.{File, PrintWriter}

class GlobalWriter(config: Config) extends GlobalWriterTrait {
  val enabled = config.writeMd5DataGlobally
  val realWriter = if (enabled) new GlobalWriterReal(config) else null

  def write(dir: File, md5s: List[Md5FileInfo]) {
    //if (Config.debugLog) println("GlobalWriter: " + dir)
    if (enabled) realWriter.write(dir, md5s)
  }

  def close() {
    if (enabled) realWriter.close()
  }

  class GlobalWriterReal(config: Config) {
    val tmpGlobalDataFile = new File(config.tmpMd5dataGlobalFilePath())
    val writer: PrintWriter = new PrintWriter(tmpGlobalDataFile, "UTF-8")

    def write(dir: File, md5s: List[Md5FileInfo]) {
      writeGlobalMd5data(writer, dir, md5s)
      if (!config.doVerify) {
        // Flush more quickly when check'ing md5's because it'll take longer anyway, and in case command is C-c'ed its worth having the extra output, plus it gives comfort when seeing it run
        writer.flush
      }
    }

    def close() {
      writer.close()
      val filename = Config.it.md5dataGlobalFilePath()
      val globalDataFile = new File(filename)
      if (globalDataFile.exists()) globalDataFile.delete();
      tmpGlobalDataFile.renameTo(globalDataFile)
    }
  }
}