import scala.io.Source.fromFile

object Day01 extends App {
  val content = fromFile("inputs/day01").toList

  def nextFloor(floor: Int, c: Char): Int = floor + (if (c == '(') 1 else -1)

  val floors = content.scanLeft(0)(nextFloor)

  println(floors.last)
  println(floors.takeWhile(_ > -1).length)

}
