import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}

class RoverSpec extends WordSpec with Matchers with BeforeAndAfterEach {

//  You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.
//  The rover receives a character array of commands.
//  Implement commands that move the rover forward/backward (f,b).
//    Implement commands that turn the rover left/right (l,r).
//    Implement wrapping from one edge of the grid to another. (planets are spheres after all)
//  Implement obstacle detection before each move to a new square.
//  If a given sequence of commands encounters an obstacle,
//  the rover moves up to the last possible point, aborts the sequence and reports the obstacle.

  var rover: Rover = _
  val givenStartingPoint = Position(x = 0, y = 0)
  val givenInitialDirection = "N"

  override def beforeEach(): Unit =
    rover = new Rover(givenStartingPoint, givenInitialDirection)

  "Rover" should {
    "start at give starting point" in {

      rover.currentPosition shouldBe givenStartingPoint
      rover.currentDirection shouldBe givenInitialDirection

    }

    "move forward on time" in {

      val commands = List("F")
      val newRover = rover.receiveCommand(commands)

      newRover.currentPosition shouldBe Position(0, 1)

    }

    "move forward two time" in {

      val commands = List("F", "F")
      val newRover = rover.receiveCommand(commands)

      newRover.currentPosition shouldBe Position(0, 2)

    }

    "move backward on time" in {

      val rover = new Rover(Position(0, 1), "N")
      val commands = List("B")
      val newRover = rover.receiveCommand(commands)

      newRover.currentPosition shouldBe Position(0, 0)

    }

    "rotate right" in {

      val commands = List("R")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "E"

    }

    "rotate right and forward" in {

      val commands = List("R", "F", "F")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "E"
      newRover.currentPosition shouldBe Position(2, 0)

    }

    "move right and backward" in {

      val rover = new Rover(Position(1, 1), "N")
      val commands = List("R", "B")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "E"
      newRover.currentPosition shouldBe Position(0, 1)

    }

    "rotate 3 times right" in {

      val commands = List("R", "R", "R")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "O"

    }

    "rotate 4 times right" in {

      val commands = List("R", "R", "R", "R")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "N"

    }

    "rotate 2 times left" in {

      val commands = List("L", "L")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "S"

    }

    "move left and forward" in {

      val rover = Rover(Position(1, 1), "N")
      val commands = List("L", "F")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "O"
      newRover.currentPosition shouldBe Position(0, 1)

    }

    "move left left and backward" in {

      val rover = Rover(Position(1, 1), "N")
      val commands = List("L", "L", "F")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "S"
      newRover.currentPosition shouldBe Position(1, 0)

    }

    "move left left and forward" in {

      val rover = Rover(Position(1, 1), "N")
      val commands = List("L", "L", "B")
      val newRover = rover.receiveCommand(commands)

      newRover.currentDirection shouldBe "S"
      newRover.currentPosition shouldBe Position(1, 2)

    }

  }

}

case class Position(x: Int, y: Int) {

  def incrementX = Position(x + 1, y)
  def incrementY = Position(x, y + 1)
  def decrementX = Position(x - 1, y)
  def decrementY = Position(x, y - 1)

}

case class Rover(currentPosition: Position, currentDirection: String) {

  def receiveCommand(commands: List[String]): Rover = commands match {
    case "F" :: _ =>
      val newPosition = Movements.moveForward(currentDirection, currentPosition)
      Rover(newPosition, currentDirection).receiveCommand(commands.tail)

    case "B" :: _ =>
      val newPosition = Movements.moveBackward(currentDirection, currentPosition)
      Rover(newPosition, currentDirection).receiveCommand(commands.tail)

    case rotation :: _ =>
      val newDirection = Rotation.rotate(currentDirection, rotation)
      Rover(currentPosition, newDirection).receiveCommand(commands.tail)

    case Nil => this
  }
}

object Movements {

  def moveBackward(currentDirection: String, currentPosition: Position): Position = currentDirection match {
    case "N" =>
      currentPosition.decrementY
    case "E" =>
      currentPosition.decrementX
    case "O" =>
      currentPosition.incrementX
    case "S" =>
      currentPosition.incrementY
  }

  def moveForward(currentDirection: String, currentPosition: Position) = currentDirection match {
    case "N" =>
      currentPosition.incrementY
    case "E" =>
      currentPosition.incrementX
    case "O" =>
      currentPosition.decrementX
    case "S" =>
      currentPosition.decrementY
  }

}

object Rotation {

  private val directions = Map("N" -> "E", "E" -> "S", "S" -> "O", "O" -> "N")
  private val inverseDirections = Map("N" -> "O", "E" -> "N", "S" -> "E", "O" -> "S")

  def rotate(currentDirection: String, commands: String) = commands match {

    case "R" =>
      directions(currentDirection)

    case "L" =>
      inverseDirections(currentDirection)

  }

}
