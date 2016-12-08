object Advent7 {
  
  def main(args:Array[String]):Unit = {

    val part1 = scala.io.Source.fromFile("input.txt").getLines.toList.
      map(_.split("[\\[\\]]").toList.
      map(_.sliding(4).toList.
      map(x => x.take(2) == x.reverse.take(2) & x(0) != x(1)).reduce(_|_))).
      map(_.zipWithIndex.groupBy(_._2%2==0)).
      map(_.toList.map(x => x._2.map(_._1)).map(_.reduce(_|_))).
      map(x => !x(0) & x(1)).count(_==true)

    val part2 = scala.io.Source.fromFile("input.txt").getLines.toList.
      map(_.split("[\\[\\]]").toList.
      map(_.sliding(3).toList.filter(x => x(0) == x(2) & x(0) != x(1))).
      zipWithIndex.groupBy(_._2%2==0).toList.map(_._2.map(_._1).reduce(_++_))).
      filterNot(x => x(0).isEmpty | x(1).isEmpty).
      map(x => (x(0).map(y => List(y(1),y(0),y(1)).mkString("")),x(1))).
      map(x => x._1.intersect(x._2)).filterNot(_.isEmpty).length

    println("Part 1: " + part1)
    println("Part 2: " + part2)   
      }
  }