package Md5Recurse

import java.io.File

/**
  * Created by Alex on 20-08-2016.
  */
object TestWalkTree extends App {
    val dir = new File("c:/temp")
    //    Sys.println(walkTree(new File("d:/temp")));
    for (f <- FileUtil.walkTree(dir)) Sys.println(f)
    for (f <- FileUtil.walkTree(dir) if f.getName.endsWith(".cfg")) Sys.println(f)
    for (f <- FileUtil.walkTree(dir) if f.isDirectory()) Sys.println(f)
}
