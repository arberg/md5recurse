package Util

import Util.WordWrap._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Alex on 28-12-2016.
  */
// https://github.com/vfarcic/TechnologyConversationsScala/blob/master/src/test/scala/com/wordpress/technologyconversations/learning/kata/solutions/WordWrapTest.scala
class WordWrapTest extends FlatSpec with Matchers {
  "WordWrap" should "return empty string is no text is specified" in {
    "".wordWrap(10) should be ("")
  }

  it should "work with a single word" in {
    "kata".wordWrap(4) should be ("kata")
  }

  it should "return wrapped text with small sample" in {
    val input = "This kata"
    val expected = "This\nkata"
    input.wordWrap(4) should be (expected)
  }

  it should "return wrapped text" in {
    val input = "This kata should be easy unless there are hidden, or not so hidden, obstacles. Let's start!"
    val expected = "This kata\nshould be\neasy unless\nthere are\nhidden, or\nnot so\nhidden,\nobstacles.\nLet's start!"
    input.wordWrap(12) should be (expected)
  }

  it should "return the same text if max length is the same as the length of the text" in {
    val input = "Lorem ipsum dolor sit amet."
    val expected = "Lorem ipsum dolor sit amet."
    input.wordWrap(input.length) should be (expected)
  }

}
