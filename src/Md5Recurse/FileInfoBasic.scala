package Md5Recurse

import java.io.File

/**
  * Created by Alex on 20-08-2016.
  */
class FileInfoBasic(lastModifiedSec: Long, size: Long, dirPathName: String, filename: String) {
  def getSize(): Long = size
  def getLastModifiedSec(): Long = lastModifiedSec
  //  def file(): File = file
  def getDirectoryPath() = dirPathName
  def getPath() = dirPathName + File.separator + filename
  def getName = filename
  private def exportData = s"$lastModifiedSec $size"
  def exportLineWithoutFile = exportData
  def exportLineFullPath = exportData + " " + getPath
  def exportLineFileName = exportData + " " + filename
  override def toString: String = exportLineFullPath
}

object FileInfoBasic {
  def create(lastModifiedSec: Long, size: Long, file: File) = {
    new FileInfoBasic(lastModifiedSec, size, file.getAbsoluteFile.getParentFile.getPath, file.getName)
  }
  def create(file: File) = {
    new FileInfoBasic(file.lastModified / 1000, file.length, file.getAbsoluteFile.getParentFile.getPath, file.getName)
  }
}