package Md5Recurse

import java.io.{File, PrintWriter}

class GlobalWriter(config: Config) extends GlobalWriterTrait {
    val enabled: Boolean = config.writeMd5DataGlobally
    val realWriter: GlobalWriterReal = if (enabled) new GlobalWriterReal(config) else null

    def write(dir: File, md5s: List[Md5FileInfo], doFlush: Boolean) {
        //if (Config.debugLog) println("GlobalWriter: " + dir)
        if (enabled) realWriter.write(dir, md5s, doFlush)
    }

    def close() {
        if (enabled) realWriter.close()
    }

    class GlobalWriterReal(config: Config) {
        val tmpGlobalDataFile = new File(config.tmpMd5dataGlobalFilePath)
        val writer: PrintWriter = new PrintWriter(tmpGlobalDataFile, "UTF-8")

        def write(dir: File, md5s: List[Md5FileInfo], doFlush: Boolean) {
            writeGlobalMd5data(writer, dir, md5s, doFlush)
        }

        def close() {
            writer.close()
            val filename = Config.it.md5dataGlobalFilePath
            val globalDataFile = new File(filename)
            if (globalDataFile.exists()) globalDataFile.delete()
            tmpGlobalDataFile.renameTo(globalDataFile)
        }
    }

}