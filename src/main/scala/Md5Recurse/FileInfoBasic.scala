package Md5Recurse

import java.io.File

/**
  * Created by Alex on 20-08-2016.
  */
class FileInfoBasic(lastModified: Long, size: Long, dirPathName: String, filename: String) {
    def getSize(): Long = size

    /**
      * Linux has lastModified in seconds and Windows (NTFS) in milliseconds.
      */
    def getLastModified(): Long = lastModified

    //  def file(): File = file
    def getDirectoryPath() = dirPathName

    def getPath() = dirPathName + File.separator + filename

    def getName = filename

    private def exportData = s"$lastModified $size"

    def exportLineWithoutFile = exportData

    def exportLineFullPath = exportData + " " + getPath

    def exportLineFileName = exportData + " " + filename

    override def toString: String = exportLineFullPath

    def createUpdateLastModified() = {
        new FileInfoBasic(new File(getPath()).lastModified(), size, dirPathName, filename)
    }
}

object FileInfoBasic {
    // Invariant: File is canonicalised
    def create(lastModified: Long, size: Long, file: File) = {
        // todo remove converter from Sec to Millis
        val lastModifiedMillis = if (lastModified < 1587194964 && lastModified == file.lastModified() / 1000) file.lastModified() else lastModified
        new FileInfoBasic(lastModifiedMillis, size, file.getParentFile.getPath, file.getName)
    }

    // Invariant: File is canonicalised
    def create(lastModified: Long, file: File) = {
        new FileInfoBasic(lastModified, file.length, file.getParentFile.getPath, file.getName)
    }

    // Invariant: File is canonicalised
    def create(file: File) = {
        new FileInfoBasic(file.lastModified(), file.length, file.getParentFile.getPath, file.getName)
    }
}