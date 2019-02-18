package goosegame

import Messages._

case class GooseGame(result: String, state: GameBoard) {

  def execute(command: Command): GooseGame = command match {

    case AddPlayer(name) =>
      state.addPlayer(Player(name)) match {

        case Right(newState) =>
          GooseGame(addedPlayerMessage(newState.playersRepository.players), newState)

        case Left(PlayerAlreadyExist(playerName)) =>
          GooseGame(playerAlreadyExistMessage(playerName), state)
      }

    case Move(name, dice1, dice2) =>
      state.movePlayer(Player(name), dice1, dice2) match {

        case Right(newState) =>
          GooseGame("", newState)

        case Left(IncorrectDiceValue(_, _)) =>
          GooseGame("", state)

      }

  }

}
