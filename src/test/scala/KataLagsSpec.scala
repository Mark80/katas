import org.scalatest.{Matchers, WordSpec}

class KataLagsSpec extends WordSpec with Matchers {

  "RentService" should {
    val range1 = Range(0, 2)
    val range2 = Range(3, 5)
    val range3 = Range(1, 5)
    val range4 = Range(1, 5)

    "check if 2 date range are overlapped" in {

      notOverlaps(range1, range2) shouldBe true
      notOverlaps(range2, range1) shouldBe true
      notOverlaps(range1, range1) shouldBe false
      notOverlaps(range1, range3) shouldBe false

    }

    "build all possible solution" in {

      allPossibleSolution(List(range1, range2)) shouldBe List(List(range1, range2))
      allPossibleSolution(List(range1, range2, range3)) shouldBe List(List(range1, range2))

    }

  }

  def allPossibleSolution(ranges: List[Range]) = {
    val sortedRanges = ranges.sortBy(_.start)

    sortedRanges match {
      case firstElement :: rest =>
        val (not, yes) = rest.partition(range => notOverlaps(range, sortedRanges.head))
        List(firstElement :: not)
      case Nil => Nil
    }
  }

  def notOverlaps(range1: Range, range2: Range) =
    range1.start > range2.end || range1.end < range2.start

}

case class RangeDate(start: Int, end: Int)
