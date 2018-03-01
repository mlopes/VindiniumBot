package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class RandomBot extends Bot {

  var lookup: Option[Lookup] = None

  def move(input: Input): bot.Dir.Value = {
    if(input.game.turn < 4) {
      lookup = Some(new Lookup(input.game.board))
    }

    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}

class Lookup(gameBoard: Board) {
  val worldMap: Vector[PositionedTile] = buildBoard(gameBoard)

  val mines: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Mine(_)) => p }

  val taverns: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Tavern) => p }

  val walls: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Wall) => p }

  def getClosedTiles(heroes: List[Hero]): Vector[Pos] = mines ++ taverns ++ walls ++ heroes.map(_.pos).toVector

  private def buildBoard(board: Board): Vector[PositionedTile] =
    board.tiles.zip((0 until board.size).flatMap(x => (0 until board.size).map(Pos(x, _)))).map {
      case (Tile.Hero(_), p: Pos) => PositionedTile(p, Tile.Air)
      case (t: Tile, p: Pos) => PositionedTile(p, t) }
}