package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class RandomBot extends Bot {

  def move(input: Input): bot.Dir.Value = {

    if(input.game.turn < 4) {
      Lookup.build(input.game.board)
    }

    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}

object Lookup {

  var mines: Vector[Pos] = Vector.empty
  var taverns: Vector[Pos] = Vector.empty
  var walls: Vector[Pos] = Vector.empty
  var worldMap: Vector[(Pos, Tile)] = Vector.empty

  def build(gameBoard: Board): Unit = {
    buildBoard(gameBoard)
    buildMinesLookup()
    buildTavernsLookup()
  }

  def getClosedTiles(heroes: List[Hero]): Vector[Pos] = mines ++ taverns ++ walls ++ heroes.map {h => h.pos}.toVector

  private def buildBoard(board: Board): Unit =
    worldMap = board.tiles.zipWithIndex.map { indexedTile: (Tile, Int) =>
      (
        Pos(indexedTile._2 / board.size, indexedTile._2 % board.size),
        indexedTile._1 match {
          case Tile.Hero(_) => Tile.Air
          case t: Tile => t })}

  private def buildTavernsLookup(): Unit = taverns = worldMap.filter {
    case (_, Tile.Tavern) => true
    case _ => false }.map {_._1}

  private def buildMinesLookup(): Unit = mines = worldMap.filter {
    case (_, Tile.Mine(_)) => true
    case _ => false }.map {_._1}

  private def buildWallsLookup(): Unit = walls = worldMap.filter {
    case (_, Tile.Wall) => true
    case _ => false }.map {_._1}

}