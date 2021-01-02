package day02

import scala.io.Source.fromFile
import scala.util.matching.Regex

object Main extends App {
  case class Dimensions(l: Int, w: Int, h: Int)
  case class Sides(lw: Int, lh: Int, wh: Int)

  val dimensionsPattern: Regex = "([0-9]+)x([0-9]+)x([0-9]+)".r

  def parseDimension(text: String): Dimensions = {
    dimensionsPattern.findFirstMatchIn(text) match {
      case Some(m) => Dimensions(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt)
      case None =>
        println(s"Error: unable to match $text")
        Dimensions(0,0,0)
    }
  }

  def calcSides(d: Dimensions): Sides = Sides(d.l * d.w, d.l * d.h, d.w * d.h)

  def min(a: Int, b: Int): Int = if (a < b) a else b
  def max(a: Int, b: Int): Int = if (a > b) a else b

  def smallestSide(s: Sides): Int = min(s.lw, min(s.lh, s.wh))

  def wrappingPaper(s: Sides): Int = 2 * s.lh + 2 * s.lw + 2 * s.wh + smallestSide(s)

  def bow(d: Dimensions): Int = {
    val s1 = min(d.l, d.w)
    val s2 = min(max(d.l, d.w), d.h)
    s1 + s1 + s2 + s2 + d.l * d.w * d.h
  }

  val contents = fromFile("src/main/scala/day02/input")
  val dims = contents.getLines().map(parseDimension).toList
  val sides = dims.map(calcSides)

  println(sides.map(wrappingPaper).sum)
  println(dims.map(bow).sum)
}
