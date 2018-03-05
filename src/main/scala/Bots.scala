package bot

import Dir._

import scala.util.Random
import Tile._
import bot.AStar.Step

trait Bot {
  def move(input: Input): Dir
}

class TheTerminator extends Bot {

  var lookupOption: Option[Lookup] = None

  def move(input: Input): Dir.Value = {

    val board: Board = input.game.board
    val hero: Hero = input.hero
    val lookup: Lookup = lookupOption.getOrElse(new Lookup(board))

    if(input.game.turn < 4) {
      println(lookup.worldMap)
      println(input.game.board.tiles)
    }

    println(s"mine at ${lookup.mines.head}")
    println(s"me at ${hero.pos}")

    val pathToNextMine = AStar.getShortestPathTo(board, hero.pos, lookup.mines.head)

    println(s"Next mine path: $pathToNextMine")

    val step = pathToNextMine.reverse.headOption match {
      case Some(s: Step) =>
        if(s.pos.y == hero.pos.y) {
          if(s.pos.x > hero.pos.x) Dir.South
          else Dir.North
        } else {
          if(s.pos.y > hero.pos.y) Dir.East
          Dir.West
        }
      case None => Dir.Stay
    }

    println(s"step to $step")

    step

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

    def loop(currentTile: Pos, cost: Int, openTiles: List[Step], closedTiles: Set[Pos], shortestPath: List[Step]): List[Step] = {

      val updatedClosedTiles = closedTiles + currentTile

      val newOpenTiles: List[AStar.Step] = currentTile.neighbors.collect {
        case p if p == destination => AStar.Step(p, cost, estimatedCost(p, destination))
        case p if board.at(p) == Some(Tile.Air) => AStar.Step(p, cost, estimatedCost(p, destination))
      }.toList.filterNot{s:Step => closedTiles.contains(s.pos) }

      if(newOpenTiles.isEmpty) {
        println(s"no new open tiles $currentTile")
        val updatedOpenTiles = (newOpenTiles ++ openTiles).collect {
          case s if !closedTiles.contains(s.pos) => s
        }

        openTiles.headOption match {
          case Some(t) => loop(t.pos, t.cost + 1, openTiles, closedTiles, shortestPath.filterNot(currentTile==))
          case None => List.empty
        }

      } else {
        val destinationTile: Option[Step] = newOpenTiles.collectFirst {
          case s@AStar.Step(d, _, _) if d == destination => s
        }

        if (destinationTile.isEmpty) {
          println(s"Still not at destination $currentTile")
          val lowestScoreStep: Step = newOpenTiles.minBy(_.score)
          val updatedCloseTiles = closedTiles + lowestScoreStep.pos
          val updatedOpenTiles = (newOpenTiles ++ openTiles).collect {
            case s if !updatedCloseTiles .contains(s.pos) => s
          }

          println(s"looping with ${lowestScoreStep.pos} added to $shortestPath")

          loop(lowestScoreStep.pos, cost + 1, updatedOpenTiles, updatedCloseTiles, lowestScoreStep :: shortestPath)
        } else {
          println(s"at destination $currentTile")
          destinationTile.toList ++ shortestPath
        }
      }
    }

    loop(origin, 1, List.empty, Set(origin), List.empty)
  }

  private def estimatedCost(origin: Pos, destination: Pos): Int =
    Math.abs(destination.x - origin.x) + Math.abs(destination.y - origin.y)

  case class Step(pos: Pos, cost: Int, estimatedCost: Int) {
    val score: Int = cost + estimatedCost
  }
}