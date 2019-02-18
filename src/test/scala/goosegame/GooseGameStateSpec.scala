package goosegame

import org.scalatest.{EitherValues, Matchers, OptionValues, WordSpec}

class GooseGameStateSpec extends WordSpec with Matchers with OptionValues with EitherValues {

  val startingBoard = GameBoard(PlayerRepository(Nil), Map.empty)
  val gooseGame = GooseGame("", startingBoard)

  val pippo = Player("Pippo")
  val pluto = Player("Pluto")

  "Goose game" should {

    "add one player" in {

      val newState = gooseGame
        .execute(AddPlayer("Pippo"))

      playerOf(newState) shouldBe List(Player("Pippo"))
      newState.result shouldBe "players: Pippo"

    }

    "move player" in {
      val newState = gooseGame
        .execute(AddPlayer("Pippo"))
        .execute(AddPlayer("Pluto"))
        .execute(Move("Pippo", 4, 2))

      positionOf(newState, pippo) shouldBe 6
      positionOf(newState, pluto) shouldBe 0

    }

    "add two player" in {

      val newGooseGame = gooseGame
        .execute(AddPlayer("Pippo"))
        .execute(AddPlayer("Pluto"))

      playerOf(newGooseGame) shouldBe List(Player("Pluto"), Player("Pippo"))
      newGooseGame.result shouldBe "players: Pluto, Pippo"

    }

    "not add player if player already exist" in {

      val newState = gooseGame
        .execute(AddPlayer("Pippo"))
        .execute(AddPlayer("Pippo"))

      playerOf(newState) shouldBe List(Player("Pippo"))
      newState.result shouldBe "Pippo: already existing player"

    }

  }

  private def positionOf(newState: GooseGame, player: Player) =
    newState.state.positionByPlayer.get(player).value

  private def playerOf(newState: GooseGame) =
    newState.state.playersRepository.players

}
