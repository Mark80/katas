package goosegame

sealed trait Command

case class AddPlayer(name: String) extends Command
case class Move(name: String, dice1: Int, dice2: Int) extends Command
