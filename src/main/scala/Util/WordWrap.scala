// Use different package than Md5Recurse to avoid intelliJ breaking import Wordwrap._ whenever there's a compile failure
package Util

// https://github.com/vfarcic/TechnologyConversationsScala/blob/master/src/test/scala/com/wordpress/technologyconversations/learning/kata/solutions/WordWrapTest.scala
object WordWrap {

    implicit class StringImprovements(s: String) {

        def wordWrap(maxLength: Int): String = {
            s.split(" ").foldLeft(Array(""))((out, in) => {
                if ((out.last + " " + in).trim.length > maxLength) {
                    out :+ in
                } else {
                    out.updated(out.size - 1, out.last + " " + in)
                }
            }).mkString("\n").trim
        }

        def wordWrap(maxLength: Int, indent: Int): String = {
            s.split(" ").foldLeft(Array(""))((out: Array[String], in: String) => {
                if ((out.last + " " + in).trim.length > maxLength) {
                    out :+ (" " * indent + in)
                } else {
                    out.updated(out.size - 1, out.last + " " + in)
                }
            }).mkString("\n").trim
        }

    }

}
