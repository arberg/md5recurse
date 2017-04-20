package Md5Recurse

import java.io.File

/**
  * Created by Alex on 20-08-2016.
  */
object TestWalkTree extends App {
  val dir = new File("c:/temp")
  //    println(walkTree(new File("d:/temp")));
  for (f <- FileUtil.walkTree(dir)) println(f)
  for (f <- FileUtil.walkTree(dir) if f.getName.endsWith(".cfg")) println(f)
  for (f <- FileUtil.walkTree(dir) if f.isDirectory()) println(f)
}
