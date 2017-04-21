package Md5Recurse

import java.io.{File, PrintWriter}

class FailureWriter(config: Config) extends GlobalWriterTrait {
    val enabled = Config.it.writeMd5DataGlobally
    var writer: Option[PrintWriter] = None
    var msgWriter: Option[PrintWriter] = None

    def init() {
      if (msgWriter.isEmpty) {
        msgWriter = Some(new PrintWriter(config.failureLogFile(), "UTF-8"))
      }
      if (writer.isEmpty) {
        writer = Some(new PrintWriter(config.failureFile(), "UTF-8"))
      }
    }

    def write(dir: File, failureMd5s: List[Md5FileInfo], failureMessages: List[String]) {
      if (enabled && !failureMessages.isEmpty) {
        init()
        for (failure <- failureMessages) {
          msgWriter.get.println(failure)
        }
        msgWriter.get.flush
        writeGlobalMd5data(writer.get, dir, failureMd5s)
        writer.get.flush
      }
    }

    def close() {
      if (writer.isDefined) {
        writer.get.close()
      }
      if (msgWriter.isDefined) {
        msgWriter.get.close()
      }
    }
  }