package Md5Recurse

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.{Map, mutable}

/**
  * Created by Alex on 20-08-2016.
  */
class FileListOrMap {

    // We store bytes instead of String because it uses roughly half the memory. It made no difference in execution time, but reduced memory
    // need from 350M to 256M of Java Heap for 110MB global file.
    private var list = new ListBuffer[Array[Byte]]
    var map: mutable.LinkedHashMap[String, Md5FileInfo] = null

    // filling map cause a blowup of memory usage as each Md5FileInfo contains the full path
    def fillMap(dir: String): Unit = {
        if (list != null) {
            map = mutable.LinkedHashMap()
            list.foreach(line => {
                try {
                    addToMapInner(Md5FileInfo.parseMd5DataLine(dir, new String(line, "UTF-8")))
                } catch {
                    case e: RuntimeException => Console.err.println("Error occurred parsing md5data line: " + line)
                }
            })
            list = null
        }
    }

    def addToList(line: String) = {
        list += line.getBytes("UTF-8")
    }

    private def addToMapInner(fileInfo: Md5FileInfo) = {
        map += (fileInfo.fileName -> fileInfo)
    }

    def addToMap(fileInfo: Md5FileInfo) = {
        if (list != null) fillMap(fileInfo.getDirectoryPath)
        addToMapInner(fileInfo)
    }
}

class DirToFileMap {
    // By using File to represent directory we get equals evaluated relative to local filesystem, not just raw string (is that true?).
    // That is more stable, but likely to be slower.
    // However our Md5SumInfo.equals is based on raw strings
    // Everything in file should have been generated by this program, so raw string comparison is enough
    val map: mutable.Map[String, FileListOrMap] = mutable.LinkedHashMap()
    var doneBuilding = false

    def getOrCreateDir(dir: File): FileListOrMap = {
        getOrCreateDir(dir.getPath)
    }

    def getOrCreateDir(dir: String): FileListOrMap = {
        val old = map.get(dir)
        if (old.isDefined) {
            old.get
        } else {
            val newFileMap = new FileListOrMap()
            map += (dir -> newFileMap)
            newFileMap
        }
    }

    private def extractMap(dir: String, maybeListOrMap: Option[FileListOrMap]) = {
        maybeListOrMap match {
            case Some(listMap) => {
                listMap.fillMap(dir)
                Some(listMap.map)
            }
            case None => None
        }
    }

    def commonGetPrepareLazy(dir: String): Option[Map[String, Md5FileInfo]] = {
        extractMap(dir, map.get(dir))
    }

    def getDir(dir: File): Option[Map[String, Md5FileInfo]] = {
        commonGetPrepareLazy(dir.getPath)
    }

    def getDir(dir: String): Option[Map[String, Md5FileInfo]] = {
        commonGetPrepareLazy(dir)
    }

    def removeDirListMap(dir: String): Option[FileListOrMap] = {
        map.remove(dir)
    }

    def removeDir(dir: String): Option[Map[String, Md5FileInfo]] = {
        extractMap(dir, map.remove(dir))
    }

    def removeDir(dirFile: File): Option[Map[String, Md5FileInfo]] = {
        val dir = dirFile.getPath
        extractMap(dir, map.remove(dir))
    }

    def setDoneBuilding() {
        doneBuilding = true
    }
}
