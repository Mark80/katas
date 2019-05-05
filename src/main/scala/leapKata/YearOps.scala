package leapKata

object YearOps {

  def isLeap(year: Int): Boolean =
    isDivisibleBy4(year) &&
      (isNotDivisibleFor100(year) ||
        isDivisibleFor400(year))

  private def isDivisibleFor400(year: Int) =
    year % 400 == 0

  private def isNotDivisibleFor100(year: Int) =
    year % 100 != 0

  private def isDivisibleBy4(year: Int) =
    year % 4 == 0
}
