package production

class Board(contents: Array[Array[Int]]) {

  private val rowDim: Int = contents.length
  private val colDim: Int = contents(0).length

  val Die = 0
  val Live = 1

  private val cells =
    for {
      col <- 0 until colDim
      row <- 0 until rowDim
    } yield (row, col)

  def tik(): Board =
    new Board(cells.foldLeft(Array.ofDim[Int](rowDim, colDim)) {
      case (newContents, (row, col)) =>
        newContents(row)(col) = nextStatusForCell(row, col)
        newContents
    })

  private def nextStatusForCell(cellRow: Int, cellCol: Int): Int = {
    val neighboursValue =
      getNeighboursCell(cellRow, cellCol).map { case (x, y) => contents(x)(y) }

    val neighboursLivingScore = neighboursValue.sum

    if (neighboursLivingScore < 2 || neighboursLivingScore > 3)
      Die
    else if (neighboursLivingScore == 3)
      Live
    else
      restTheSame(cellRow, cellCol)

  }

  private def restTheSame(cellRow: Int, cellCol: Int) =
    contents(cellRow)(cellCol)

  private def getNeighboursCell(cellRow: Int, cellCol: Int) =
    for {
      y <- -1 to 1
      x <- -1 to 1
      if itNotItSelf(y, x) && isInTheBoard(cellRow, cellCol, y, x)
    } yield (cellRow + x, cellCol + y)

  private def isInTheBoard(cellRow: Int, cellCol: Int, y: Int, x: Int) =
    ((cellRow + x >= 0) && (cellCol + y >= 0)) && ((cellRow + x <= 2) && (cellCol + y <= 2))

  private def itNotItSelf(col: Int, row: Int) =
    (col, row) != (0, 0)

  def update(col: Int, row: Int, value: Int): Unit =
    contents(row)(col) = value

  override def toString: String = {
    val result = for {
      col <- 0 until colDim
      row <- 0 until rowDim
      cell = if (row == (rowDim - 1) && col != colDim - 1) contents(row)(col) + "\n" else contents(row)(col).toString
    } yield cell

    result.mkString
  }

}
