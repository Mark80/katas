package trimLine

import org.scalatest.{Matchers, WordSpec}

class TrimmingLineSpec extends WordSpec with Matchers {

  "Trimmer" should {

    "leave unchanged well string" in {

      trimEndOflLine("abc") shouldBe "abc"

    }

    "remove empty space at the end of the line" in {

      trimEndOflLine("abc ") shouldBe "abc"

    }

    "don't remove empty space at starting of the line" in {

      trimEndOflLine(" abc") shouldBe " abc"

    }

    "remove empty space only at the end of the line" in {

      trimEndOflLine(" abc ") shouldBe " abc"

    }

    "remove multiple empty spaces at the end of the line" in {

      trimEndOflLine("abc  ") shouldBe "abc"

    }

    "remove tab space" in {

      trimEndOflLine("abc\t") shouldBe "abc"
    }

    "remove tab space and space in the same line" in {

      trimEndOflLine("  abc\t ") shouldBe "  abc"
    }

    "don't remove empty space and tab in the middle of a line" in {

      trimEndOflLine("abc  \td  \t") shouldBe "abc  \td"
    }

  }

  def trimEndOflLine(line: String) = {
    val numberOfEndingEmptySpace = numberOfEmptySpace(line)
    line.substring(0, line.length - numberOfEndingEmptySpace) + line.substring(line.length - numberOfEndingEmptySpace, line.length).trim
  }

  private def numberOfEmptySpace(line: String): Int =
    line.reverse.takeWhile((c: Char) => c.isSpaceChar || (c == '\t')).length
}
