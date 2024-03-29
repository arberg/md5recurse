package Md5Recurse

import java.io.File

/**
  * Created by Alex on 20-08-2016.
  */
class FileInfoBasic(lastModified: Long, size: Long, dirPathName: String, filename: String) {
    // Equals timestamps except for rounding error due to milliSeconds, but then one must be with zero-milliseconds (caused by different precisions)
    private def isEqualTimeExceptMillisPrecision(time1: Long, time2: Long) = time1 == time2 || time1 / 1000 == time2 / 1000 && (time1 % 1000 == 0 || time2 % 1000 == 0)

    def isLastModifiedEqualTo(other: FileInfoBasic) = isEqualTimeExceptMillisPrecision(lastModified, other.getLastModified())
    def isLastModifiedEqualTo(otherLastModified: Long) = isEqualTimeExceptMillisPrecision(lastModified, otherLastModified)

    def getSize(): Long = size

    /**
      * Linux has lastModified in seconds and Windows (NTFS) in milliseconds.
      */
    def getLastModified(): Long = lastModified

    def getLastModifiedString(): String = DateUtil.timeToStr(lastModified)

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
    def create(lastModifiedMillis: Long, size: Long, file: File) = {
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