package day03

import scala.collection.immutable.Set
import scala.io.Source.fromFile

object Main extends App {
  case class Vector(x: Int, y: Int) {
    def +(that: Vector): Vector = Vector(this.x + that.x, this.y + that.y)
  }

  case class Santa(pos: Vector, visited: Set[Vector]) {
    def move(c: Char): Santa = {
      val newPos = c match {
        case '^' => this.pos + Vector(0, 1)
        case '>' => this.pos + Vector(1, 0)
        case 'v' => this.pos + Vector(0, -1)
        case '<' => this.pos + Vector(-1, 0)
      }
      Santa(newPos, this.visited + newPos)
    }
  }

  val file = fromFile("src/main/scala/day03/input")
  val moves = file.toList
  val startSanta = Santa(Vector(0,0), Set.empty)
  file.close()

  val santa1 = moves.foldLeft(startSanta)(_ move _)
  println(santa1.visited.size)

  val (santaMoves, robotMoves) = moves.zipWithIndex.partition(x => x._2 % 2 == 0)
  val santa2 = santaMoves.map(_._1).foldLeft(startSanta)(_ move _)
  val robot = robotMoves.map(_._1).foldLeft(startSanta)(_ move _)
  println(santa2.visited.union(robot.visited).size)
}
