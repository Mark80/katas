package goosegame

case class PlayerRepository(players: List[Player]) {

  def isNewPlayer(player: Player) =
    !players.contains(player)

  def addNewPlayer(player: Player): PlayerRepository =
    PlayerRepository(player :: players)

}
