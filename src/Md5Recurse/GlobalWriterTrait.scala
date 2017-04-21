package Md5Recurse

import java.io.{File, PrintWriter}

trait GlobalWriterTrait {

  def writeGlobalMd5data(writer: PrintWriter, dirPath: File, md5s: List[Md5FileInfo], doFlush: Boolean) {
    if (md5s.size > 0) {
      writer.println(">" + dirPath.getPath);
      for (md5 <- md5s) {
        writer.println(md5.exportDataLineFileName);
      }
      if (doFlush)
        writer.flush
    }
  }

}