package Md5Recurse

/**
  * Created by Alex on 20-08-2016.
  */
class Md5SumInfo(m: String, b: Boolean, f: String) {
  val md5 = m
  val isBinary = b
  val filepath = f
  override def toString() = {
    md5 + " " + (if (isBinary) "*" else " ") + f
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Md5SumInfo => md5 == that.md5 && this.isBinary == isBinary && filepath == that.filepath
      case _                => false
    }
}
