import org.scalatest._
import production.Board

class GameOfLifeSpec extends WordSpec with Matchers {

  /*
  Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
  Any live cell with more than three live neighbours dies, as if by overcrowding.
  Any live cell with two or three live neighbours lives on to the next generation.
  Any dead cell with exactly three live neighbours becomes a live cell.
   */

  "A 3x3 board" should {

    "start with all zero" in {

      val board = new Board(Array.ofDim(3, 3))

      val expected =
        """000
          |000
          |000""".stripMargin

      board.toString shouldBe expected

    }

    "if no live cell exist stay the same" in {

      val board = new Board(Array.ofDim(3, 3))

      val newBoard = board.tik()

      val expected =
        """000
          |000
          |000""".stripMargin

      newBoard.toString shouldBe expected

    }

    "if only one live cell exist will die" in {

      val board = new Board(Array.ofDim(3, 3))

      board(1, 1) = 1

      val expected1 =
        """000
          |010
          |000""".stripMargin

      board.toString shouldBe expected1

      val newBoard = board.tik()

      val expected =
        """000
          |000
          |000""".stripMargin

      newBoard.toString shouldBe expected

    }

    "Any live cell with two or three live neighbours lives on to the next generation" in {

      val board = new Board(Array.ofDim(3, 3))

      board(1, 1) = 1
      board(0, 0) = 1
      board(2, 2) = 1

      val expected1 =
        """100
          |010
          |001""".stripMargin

      board.toString shouldBe expected1

      val newBoard = board.tik()

      val expected =
        """000
          |010
          |000""".stripMargin

      newBoard.toString shouldBe expected

    }

    "Any live cell with more than three live neighbours dies, as if by overcrowding" in {

      val board = new Board(Array.ofDim(3, 3))

      board(1, 1) = 1
      board(0, 0) = 1
      board(2, 2) = 1
      board(2, 0) = 1
      board(0, 2) = 1

      val expected1 =
        """101
          |010
          |101""".stripMargin

      board.toString shouldBe expected1

      val newBoard = board.tik()

      val expected =
        """010
          |101
          |010""".stripMargin

      newBoard.toString shouldBe expected

    }

    "Any dead cell with exactly three live neighbours becomes a live cell" in {
      val cells = Array.ofDim[Int](3, 3)

      cells(0)(1) = 1
      cells(1)(1) = 1
      cells(2)(1) = 1

      val board = new Board(cells)

      val expected1 =
        """000
          |111
          |000""".stripMargin

      board.toString shouldBe expected1

      val newBoard = board.tik()

      val expected =
        """010
          |010
          |010""".stripMargin

      newBoard.toString shouldBe expected

    }

  }

}
