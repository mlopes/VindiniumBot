package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class TheTerminator extends Bot {

  var lookupOption: Option[Lookup] = None

  def move(input: Input): Dir.Value = {

    val board: Board = input.game.board
    val hero: Hero = input.hero
    val lookup: Lookup = lookupOption.getOrElse(new Lookup(board))

    println(hero.pos)
    println(lookup.mines.head)
    println(AStar.getShortestPathTo(board, hero.pos, lookup.mines.head))

    Dir.Stay
    //    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
    //      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    //    }
    //  } getOrElse Dir.Stay
  }
}

class Lookup(gameBoard: Board) {
  val worldMap: Vector[PositionedTile] = buildBoard(gameBoard)

  val mines: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Mine(_)) => p }

  val taverns: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Tavern) => p }

  val walls: Vector[Pos] = worldMap.collect {
    case PositionedTile(p: Pos, Tile.Wall) => p }

  def nonWalkableTiles(heroes: List[Hero]): Vector[Pos] = mines ++ taverns ++ walls ++ heroes.map(_.pos).toVector

  private def buildBoard(board: Board): Vector[PositionedTile] =
    board.tiles.zip((0 until board.size).flatMap(x => (0 until board.size).map(Pos(x, _)))).map {
      case (Tile.Hero(_), p: Pos) => PositionedTile(p, Tile.Air)
      case (t: Tile, p: Pos) => PositionedTile(p, t) }
}

object AStar {

  def getShortestPathTo(board: Board, origin: Pos, destination: Pos): List[Step] = {

    def loop(currentTile: Pos, cost: Int, parent: Option[Step], openTiles: List[Step], closedTiles: List[Pos], shortestPath: List[Step]): List[Step] = {
      val newOpenTiles: List[AStar.Step] = currentTile.neighbors.collect {
        case p if p == destination => AStar.Step(p, cost, estimatedCost(p, destination), parent)
        case p if board.at(p) == Some(Tile.Air) => AStar.Step(p, cost, estimatedCost(p, destination), parent)
      }.toList


      val destinationTile: List[Step] = newOpenTiles.collect {
        case s@AStar.Step(d, _, _, _) if d == destination => s
      }

      if(destinationTile.isEmpty)
      {
        val lowestScoreStep: Step = newOpenTiles.minBy(_.score)
        val updatedOpenTiles = (openTiles ++ newOpenTiles).filterNot(lowestScoreStep==)

        loop(lowestScoreStep.pos, cost + 1, Some(lowestScoreStep), updatedOpenTiles, lowestScoreStep.pos :: closedTiles, lowestScoreStep :: shortestPath)
      } else {
        destinationTile ++ shortestPath
      }
    }

    loop(origin, 1, None, List.empty, List(origin), List.empty)
  }

  private def estimatedCost(origin: Pos, destination: Pos): Int =
    Math.abs(destination.x - origin.x) + Math.abs(destination.y - origin.y)

  case class Step(pos: Pos, cost: Int, estimatedCost: Int, parent: Option[Step]) {
    val score: Int = cost + estimatedCost
  }
}