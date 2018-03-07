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

    val notMyMines = lookup.mines.filter { p =>
      board.at(p) match {
        case Some(Mine(owner: Option[Int])) if owner.getOrElse(0) == hero.id => false
        case _ => true
      }}.toList

    println(s"mine at $notMyMines")
    println(s"me at ${hero.pos}")

    if(hero.life <= 25 ) {
      val pathToNextPub = AStar.getShortestPathTo(board, hero.pos, lookup.taverns.toList)

      println(s"Next pub path: $pathToNextPub")

      translatePosToMovement(hero.pos, pathToNextPub)
    } else {

      val pathToNextMine = AStar.getShortestPathTo(board, hero.pos, notMyMines)

      translatePosToMovement(hero.pos, pathToNextMine)
    }
  }

  def translatePosToMovement(origin: Pos, destination: Option[Step]): Dir = destination match {
    case Some(s: Step) =>
      println(s"going to ${s.pos}")
      if (s.pos.y == origin.y) {
        if (s.pos.x > origin.x) Dir.South
        else Dir.North
      } else {
        if (s.pos.y > origin.y) Dir.East
        else Dir.West
      }
    case None => Dir.Stay
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

  def getShortestPathTo(board: Board, origin: Pos, destinations: List[Pos]): Option[Step] = {

    val destination = origin

    def loop(originalOpenTiles: List[Step], originalClosedTiles: Set[Pos]): Option[Step] = {

      val currentTile = originalOpenTiles.minBy(_.score)
      val closedTiles = originalClosedTiles + currentTile.pos

      val newTiles: List[AStar.Step] = currentTile.pos.neighbors.collect {
        case p if p == destination =>
          AStar.Step(p, currentTile.cost + 1, estimatedCost(p, destination), Some(currentTile))
        case p if board.at(p) == Some(Tile.Air) =>
          AStar.Step(p, currentTile.cost + 1, estimatedCost(p, destination), Some(currentTile))
      }.toList

      val replacedOpenTiles = originalOpenTiles.map { tile =>
        newTiles.collectFirst {
          case newTile: Step if newTile.pos == tile.pos =>
            if (newTile.score < tile.score) {
              newTile
            } else {
              tile
            }
        } match {
          case Some(t) => t
          case _ => tile
        }
      }

      val openTiles = (newTiles.filterNot(n => replacedOpenTiles.map(_.pos).contains(n.pos)) ++ replacedOpenTiles).filterNot{s:Step => closedTiles.contains(s.pos) }

      openTiles.collectFirst {
        case AStar.Step(d, _, _, Some(p)) if d == destination => p
      } match {
        case None => openTiles match {
          case Nil => None
          case o => loop(o, closedTiles)
        }
        case Some(step) => Some(step)
      }
    }

    loop(destinations.map {o => Step(o, 1, estimatedCost(o, destination), None)}, Set())
  }

  private def estimatedCost(origin: Pos, destination: Pos): Int =
    Math.abs(destination.x - origin.x) + Math.abs(destination.y - origin.y)

  case class Step(pos: Pos, cost: Int, estimatedCost: Int, parent: Option[Step]) {
    val score: Int = cost + estimatedCost
  }
}