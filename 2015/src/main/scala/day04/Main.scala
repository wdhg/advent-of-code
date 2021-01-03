package day04

import java.security.MessageDigest

object Main extends App {

  val secret = "ckczppom"

  val md5 = MessageDigest.getInstance("MD5")

  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString

  def invalid(num: Int, zeros: Int): Boolean = {
    val input = secret + num.toString
    val hash = md5.digest(input.getBytes)
    toHex(hash).takeWhile(_ == '0').length < zeros
  }

  println(LazyList.from(1).dropWhile(invalid(_, 5)).head)
  println(LazyList.from(1).dropWhile(invalid(_, 6)).head)

}
