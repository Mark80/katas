package goosegame

object Messages {

  def addedPlayerMessage(players: List[Player]): String =
    s"players: ${players.map(_.name).mkString(", ")}"

  def playerAlreadyExistMessage(name: String): String =
    s"$name: already existing player"

}
