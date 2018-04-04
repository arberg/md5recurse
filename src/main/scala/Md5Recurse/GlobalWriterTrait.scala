package Md5Recurse

import java.io.{File, PrintWriter}

trait GlobalWriterTrait {

    def writeGlobalMd5data(writer: PrintWriter, dirPath: File, md5s: List[Md5FileInfo], doFlush: Boolean, relativizePath: Boolean, globaldir: File) {
        if (md5s.nonEmpty) {
            val dirPathValue =
                if (relativizePath) better.files.File(globaldir.getPath).relativize(better.files.File(dirPath.getPath))
                else dirPath.getPath
            writer.println(">" + dirPathValue);
            for (md5 <- md5s) {
                writer.println(md5.exportDataLineFileName);
            }
            if (doFlush) {
                writer.flush
            }
        }
    }

}