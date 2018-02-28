package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class RandomBot extends Bot {

  var mines: Vector[(Pos, Tile)] = Vector.empty
  var taverns: Vector[(Pos, Tile)] = Vector.empty
  var worldMap: Vector[(Pos, Tile)] = Vector.empty

  def move(input: Input): bot.Dir.Value = {

    if(input.game.turn < 4) {
      buildLookups(input.game.board)
    }

    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay


  private def buildLookups(gameBoard: Board): Unit = {
    buildBoard(gameBoard)
    buildMinesLookup
    buildTavernsLookup
  }

  private def buildBoard(board: Board): Unit =
    worldMap = board.tiles.zipWithIndex.map { indexedTile: (Tile, Int) =>
      (
        Pos(indexedTile._2 / board.size, indexedTile._2 % board.size),
        indexedTile._1 match {
          case Tile.Hero(_) => Tile.Air
          case t: Tile => t })}

  private def buildTavernsLookup: Unit = taverns = worldMap.filter {
      case (_, Tile.Tavern) => true
      case _ => false }

  private def buildMinesLookup: Unit = mines = worldMap.filter {
      case (_, Tile.Mine(owner)) => true
      case _ => false }
}
