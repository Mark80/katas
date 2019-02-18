package goosegame

case class GameBoard(playersRepository: PlayerRepository, positionByPlayer: Map[Player, Int]) {

  def addPlayer(player: Player): Either[Error, GameBoard] =
    if (playersRepository.isNewPlayer(player))
      Right(newStateWith(player))
    else
      Left(PlayerAlreadyExist(player.name))

  private def newStateWith(player: Player) =
    GameBoard(playersRepository = playersRepository.addNewPlayer(player), positionByPlayer = startPosition(player))

  private def startPosition(player: Player) =
    positionByPlayer + (player -> 0)

  def movePlayer(player: Player, dice1: Int, dice2: Int): Either[Error, GameBoard] =
    if (isValidDiceValue(dice1) && isValidDiceValue(dice2))
      Right(GameBoard(playersRepository, updatePosition(player, dice1 + dice2)))
    else
      Left(IncorrectDiceValue(dice1, dice2))

  private def isValidDiceValue(dice1: Int) =
    1 <= dice1 && dice1 <= 6

  private def updatePosition(player: Player, diceValue: Int): Map[Player, Int] =
    positionByPlayer + (player -> (positionByPlayer.getOrElse(player, 0) + diceValue))
}

sealed trait Error

case class PlayerAlreadyExist(name: String) extends Error
case class IncorrectDiceValue(value1: Int, value2: Int) extends Error
