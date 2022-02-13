package Md5Recurse

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.file.attribute.{BasicFileAttributes, UserDefinedFileAttributeView}
import java.nio.file.{Files, LinkOption, Paths}

import scalax.file.Path

import scala.collection.{Iterable, Iterator, immutable}

/**
  * Created by Alex on 20-08-2016.
  */
object FileUtil {
    var silenceReadErrors = false

    /** returns true if junction / reparsePoint */
    def isWinHardLink(file: File) = {
        val path = Paths.get(file.getAbsolutePath)
        val attr = Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
        import java.nio.file.attribute.DosFileAttributes
        var isReparsePoint = false
        if (classOf[DosFileAttributes].isInstance(attr)) {
            try {
                val m = attr.getClass.getDeclaredMethod("isReparsePoint")
                m.setAccessible(true)
                isReparsePoint = m.invoke(attr).asInstanceOf[Boolean]
            } catch {
                case e: Exception => Sys.println("Failed to read if link " + file)
            }
        }
        isReparsePoint
    }

    def isSymLink(file: File) = {
        val canon =
            if (file.getParent() == null) {
                file;
            } else {
                val canonDir = file.getParentFile().getCanonicalFile();
                new File(canonDir, file.getName());
            }
        val symlink = canon.getCanonicalFile() != canon.getAbsoluteFile()
        if (symlink && !Config.it.quiet) Sys.println("symlink skipped " + file)
        symlink
    }

    def getListOfFiles(dir: File): List[File] = {
        listFiles(dir).filter(_.isFile).toList
    }

    def traverse(dir: File, proc: File => Unit) {
        listFiles(dir, true) foreach { f => if (f.isDirectory) traverse(f, proc) else proc(f) }
    }

    def listFiles(dir: File, printOnError : Boolean = false): Array[File] = {
        // See comments here, and read java-doc of listFiles: https://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
        Option(dir.listFiles) match {
            case Some(f) => f
            case None => {
                if (printOnError) Console.err.println("Unable to list files in " + dir)
                Array[File]()
            }
        }
    }

    // see also http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala#
    def walkTree(file: File): Iterable[File] = {
        val children = new Iterable[File] {
            def iterator = if (file.isDirectory) listFiles(file).iterator else Iterator.empty
        }
        Seq(file) ++: children.flatMap(walkTree(_))
    }

    def logFileTimestamps(file: File) {
        val attr = Files.readAttributes(file.toPath, classOf[BasicFileAttributes])
        Sys.println("creationTime: " + attr.creationTime)
        Sys.println("lastAccessTime: " + attr.lastAccessTime)
        Sys.println("lastModifiedTime: " + attr.lastModifiedTime)
    }

    def attrView(file: File): UserDefinedFileAttributeView = {
        Files.getFileAttributeView(file.toPath, classOf[UserDefinedFileAttributeView]);
    }

    def doWithLockedFile[T](file: File, onFailureResponse: T)(supplier: () => T): T = {
        // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
        // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
        try {
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
                    if (!silenceReadErrors) Console.err.println("Failed to lock file for reading: " + file)
                    onFailureResponse
                }
            } finally {
                in.close();
            }
        } catch {
            case ioe: java.io.IOException => {
                if (!silenceReadErrors) Console.err.println("Failed to lock file for reading: " + file)
                onFailureResponse
            }
        }
    }

    def doWithLockedFile[T](file: File)(supplier: () => Unit) {
        // Lock the file so others cannot write file, while we read it to generate MD5 and then write fileAttribute
        // We don't need the lock on linux, unless the filesystem is mounted NTFS I think. Its probably filesystem dependent not OS dependent.
        try {
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
                    if (!silenceReadErrors) Console.err.println("Failed to lock file for reading: " + file)
                }
            } finally {
                in.close();
            }
        } catch {
            case ioe: java.io.IOException => if (!silenceReadErrors) Console.err.println("Failed to lock file for reading: " + file)
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
                if (!silenceReadErrors) Console.err.println("Unable to get file attribute for :" + e);
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
            case e: Exception => {
                if (!silenceReadErrors) Console.err.println("Unable to update file attribute for :" + e);
                false
            }
        }
    }

    def deleteAttr(attrView: UserDefinedFileAttributeView, name: String) {
        try {
            if (attrView.list().contains(name)) {
                attrView.delete(name);
                Sys.println("deleted attribute")
            }
        } catch {
            case e: Exception => if (!silenceReadErrors) Console.err.println("Unable to delete file attribute for :" + e)
        }
    }
}
