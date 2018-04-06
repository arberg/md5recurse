MD5Recurse
==========
MD5Recurse is a tool for generating and verifying MD5 checksums recursively for all files in a set of folders. It can

* generate MD5 checksums for all files, 
* or just update for files with newer lastModified timestamps since last scan,
* and check whether checksums are valid for all files with unchanged lastModified timestamps.

It can write MD5 checksums to a global text-file which is sorted alphabetically by filename and folder and thus enabled manual text-comparisons in order to check of which checksums and last modification timestamps differ.

MD5Recurse supports the following storage formats
* Local md5data text file per directory (in MD5Recurse own format)
* Local md5sum text file per directory (in md5sum format) - MD5Recurse only writes these files, it does not support reading the files for checksums
* Global md5data text files
* custom file attributes where the checksum and last modified timestamp is written to each file separately. Such attributes are preserved with file-rename and internal filesystem move, but not always when files are copied.

MD5Recurse is written in Scala, and thus runs in Java JVM. It is tested for Windows and Linux.

UnRaid Scripts
==========
Additional scripts have been included for generating MD5 files on the UnRaid operating system. In UnRaid each file is located on one specific disk (named cache, disk1-diskN). UnRaid has a virtual folder called 'user' which contains the union of all files on the disks. 
The script scans all disks separately and creates a global-md5data file for each disk. It also scans the virtual user folder and generates a global md5data file.

UnRaid Forum Thread
==========

[unRAID forum Thread](https://lime-technology.com/forums/topic/70736-md5recurse-hash-program-built-for-unraid/)

### Usages
* Should a disk fail the global-disk files can be used to verify a possible restore of the disk. 
* Should a subfolder in the common user-folder be lost and later restored, then the global user-md5data can be used to verify the content of the restored folder.

MD5 vs SHA-1 Considerations
==========
The tool only supports MD5, not other hashes such as SHA-1. There are known collissions in the MD5 checksum but not in the SHA-1 checksum. The rationale for using MD5 is the following. If a files content is altered for instance due to bitrot or bad restore then that change is to some extent random, it is not constructed by a human trying to subvert the security. Thus the probability that the new files MD5 checksum will be equal to the old checksum is roughly the probability that two random files have the same checksum. And that is not very likely to say the least, see [StackOverflow: How many random elements before MD5 produces collisions?](http://stackoverflow.com/questions/201705/how-many-random-elements-before-md5-produces-collisions).

Runtime Environment
==========
The Md5Recurse programs needs Java 1.8 or newer installed, and probably JAVA_HOME defined in the environment. 

Build
==========
The projects is built with gradle. Run `gradle build`. Distribution will be placed in build\distributions.

The project can be imported into IntelliJ using IntelliJ import command. Choose import as Gradle project.