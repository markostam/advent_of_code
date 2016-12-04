
import scala.io.Source

// val textLoc = "/Users/markostamenovic/code/adventOfCode/Day4/input_day4.txt"

object Advent4 {

  def getRooms (textLoc : String) = {
    Source.fromFile(textLoc).getLines.toArray.
    map(_.replace("-","")).map(_.dropRight(1).split("\\[")).
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

  implicit def bool2int(b:Boolean) = if (b) 1 else 0

  def main(args:Array[String]):Unit = {
    val rooms = getRooms(args(0))
    val pairs = rooms.map(x => (x(1),top5Words(countWords(x(0))),x(2).toInt)).
                map(x => if (x._1 == x._2) x._3 else 0).reduce(_+_)
    println("First part: " + pairs)
  }
}