object Advent6 {

  def main(args:Array[String]):Unit = {
    scala.io.Source.fromFile(args(0)).getLines.toList.transpose.
      map(_.groupBy(_.toChar)).map(x => x.map(y => (y._1, y._2.length))).
      map(x => (x.maxBy(_._2),x.minBy(_._2))).
      map(x => x._1._1.toString+x._2._1.toString).
      transpose.map(_.mkString("")).foreach(println("Answer: "))

    }
}
