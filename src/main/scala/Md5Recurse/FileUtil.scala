package Md5Recurse

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, UserDefinedFileAttributeView}

import scala.collection.{Iterable, Iterator}

/**
  * Created by Alex on 20-08-2016.
  */
object FileUtil {

  def isSymLink(file: File) = {
    val canon =
      if (file.getParent() == null) {
        file;
      } else {
        val canonDir = file.getParentFile().getCanonicalFile();
        new File(canonDir, file.getName());
      }
    val symlink = canon.getCanonicalFile() != canon.getAbsoluteFile()
    if (symlink && !Config.it.quiet) println("symlink skipped " + file)
    symlink
  }

  def getListOfFiles(dir: File): List[File] = {
    if (dir.exists && dir.isDirectory) {
      dir.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def traverse(dir: File, proc: File => Unit): Unit =
    dir.listFiles foreach { f => if (f.isDirectory) traverse(f, proc) else proc(f) }

  // see also http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala#
  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }

  def logFileTimestamps(file: File) {
    val attr = Files.readAttributes(file.toPath, classOf[BasicFileAttributes])
    System.out.println("creationTime: " + attr.creationTime)
    System.out.println("lastAccessTime: " + attr.lastAccessTime)
    System.out.println("lastModifiedTime: " + attr.lastModifiedTime)
  }

  def attrView(file: File): UserDefinedFileAttributeView = {
    Files.getFileAttributeView(file.toPath, classOf[UserDefinedFileAttributeView]);
  }

  def doWithLockedFile[T](file: File, onFailureResponse: T)(supplier: () => T): T = {
    // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
    // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
    val in = new FileInputStream(file);
    try {
      val lock = in.getChannel().tryLock(0L, Long.MaxValue, true)
      if (lock != null) {
        try {
          supplier.apply()
        } finally {
          lock.release();
        }
      } else {
        System.err.println("Failed to lock file for reading: " + file)
        onFailureResponse
      }
    } finally {
      in.close();
    }
  }

  def doWithLockedFile[T](file: File)(supplier: () => Unit) {
    // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
    // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
    val in = new FileInputStream(file);
    try {
      val lock = in.getChannel().tryLock(0L, Long.MaxValue, true)
      if (lock != null) {
        try {
          supplier.apply()
        } finally {
          lock.release();
        }
      } else {
        System.err.println("Failed to lock file for reading: " + file)
      }
    } finally {
      in.close();
    }
  }

  // In Linux: getfattr --name=user.md5recurse --only-values --absolute-names file
  // user will automatically be prefixed by java
  def getAttr(attrView: UserDefinedFileAttributeView, name: String): Option[String] = {
    try {
      if (attrView.list().contains(name)) {
        val buf: ByteBuffer = ByteBuffer.allocate(attrView.size(name));
        attrView.read(name, buf);
        buf.flip();
        Some(new String(buf.array(), "utf-8"));
      } else {
        None
      }
    } catch {
      // Thrown if no attribute by the name found
      case ioe: java.nio.file.NoSuchFileException => None
      case e: Exception => {
        System.err.println("Unable to get file attribute for :" + e);
        None
      }
    }
  }

  /**
    *
    * @param attrView
    * @param name name excluding 'user.' prefix - it is prefixed by java or linux filesystem
    * @param value
    */
  def setAttr(attrView: UserDefinedFileAttributeView, name: String, value: String) = {
    try {
      attrView.write(name, ByteBuffer.wrap(value.getBytes));
      true
    } catch {
      case e: Exception => { System.err.println("Unable to update file attribute for :" + e); false }
    }
  }

  def deleteAttr(attrView: UserDefinedFileAttributeView, name: String) {
    try {
      if (attrView.list().contains(name)) {
        attrView.delete(name);
      }
    } catch {
      case e: Exception => System.err.println("Unable to delete file attribute for :" + e)
    }
  }
}