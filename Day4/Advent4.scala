
import scala.io.Source

object Advent4 {

  def getRooms (textLoc : String) = {
    Source.fromFile(textLoc).getLines.toArray.
    map(_.dropRight(1).split("\\[")).
    map(x => Array(x(0).filter(!_.isDigit),x(1),x(0).filter(_.isDigit)))
  }

  def countWords (hash : String) = {
    hash.groupBy(_.toChar).map(x => (x._1,x._2.mkString.size))
  }

  def top5Words (countMap: Map[Char,Int]) = {
    //get top 5 words sorted by order
    countMap.toArray.sortBy(_._2).reverse.
    groupBy(_._2).toArray.sortBy(_._1).reverse.
    map(x => x._2.sortBy(_._1)).flatten.
    map(_._1.toString).take(5).reduce(_+_)
  }

  def decodeChar (letter:Char, shift:Int) : Char = {
    val alphabet = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    val start_index = alphabet.indexOf(letter)
    val out_index = (shift % 26 + start_index) % 26
    alphabet(out_index)
  }

  def main(args:Array[String]):Unit = {
    val rooms = getRooms(args(0)).map(x => Array(x(0).replace("-",""),x(1)))
    val pairs = rooms.map(x => (x(1),top5Words(countWords(x(0))),x(2).toInt)).
                map(x => if (x._1 == x._2) x._3 else 0).reduce(_+_)
    println("First part: " + pairs)

    val codes = getRooms(args(0)).map(x => (x(0).split("-"),x(2)))
    val decoded = codes.map(x => ((x._1.map(y => y.
                  map(z => decodeChar(z,x._2.toInt)))).reduce(_+" "+_), x._2)).toMap
    val queryRoom = "northpole object storage"
    println("Second part: " + decoded(queryRoom))
  }
}