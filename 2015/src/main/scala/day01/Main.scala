package day01

import scala.io.Source.fromFile

object Main extends App {
  val content = fromFile("src/main/scala/day01/input").toList

  def nextFloor(floor: Int, c: Char): Int = floor + (if (c == '(') 1 else -1)

  val floors = content.scanLeft(0)(nextFloor)

  println(floors.last)
  println(floors.takeWhile(_ > -1).length)

}
