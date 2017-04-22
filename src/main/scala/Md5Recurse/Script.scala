package Md5Recurse

import scala.collection.Seq
import scala.sys.process.{ProcessLogger, _}

/** Run a command, collecting the stdout, stderr and exit status
  *
  * For onelines use: import scala.sys.process._ and s"mv foo bar".!

  * */
object Script {
  def run(in: Seq[String]): (List[String], List[String], Int) = {
    val cmd = in.filterNot(_.isEmpty())
    var out = List[String]()
    var err = List[String]()
    val exit = cmd ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    (out.reverse, err.reverse, exit)
  }

  def run(in: String): (List[String], List[String], Int) = {
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)

    (out.reverse, err.reverse, exit)
  }
}
