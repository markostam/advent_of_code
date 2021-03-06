
object Advent8_breeze {

  def main(args:Array[String]):Unit = {

    var display = Array.fill(6,50)(" ")
    val input = scala.io.Source.fromFile(args(0)).getLines.toList

    val prob1 = input.filter(_.split(" ")(0) == "rect").
      map(x => x.split(" ")).map(_(1).split("x")).
      map(x => x(1).toInt*x(0).toInt).reduce(_+_)

    def roll2 (a : Array[String], n : Int) = {
      // function to roll values around an array like np.roll
      val len = a.size
      var newA = Array.fill(len)(".")
      for (i <- n to len-1) {
        newA(i) = a(i-n)
      }
      for (i <- 0 to n-1) {
        newA(i) = a(len-n+i)
      }
      newA
    }

    for (line <- input) {
        val lineSplit = line.split(" ")
        lineSplit(0) match {
          case "rect" => {
                println(line)
                val colRow = lineSplit(1).split("x").map(_.toInt)
                for (i <- 0 to colRow(1)){
                  for (j <- 0 to colRow(0)){
                    display(i)(j) = "#"
                  }
                }
                display
            }
          case "rotate" => {
            lineSplit(1) match {
              case "row" => {
                  val row = lineSplit(2).split("=")(1).toInt
                  val shift = lineSplit.last.toInt
                  println(line)
                  display(row) = roll2(display(row),shift)
                  display
              }
              case "column" => {
                  val column = lineSplit(2).split("=")(1).toInt
                  val shift = lineSplit.last.toInt
                  println(line)
                  display = display.transpose
                  display(column) = roll2(display(column),shift)
                  display = display.transpose
                  display
                }
              case _ => {
                print("something is weird man")
              }
            }
          }
          case _ => {
            print("something is weird man")
          }
        }
    }

    println("problem 1: " + prob1)
    println("problem 2: ")
    display.foreach(x => println(x.toList))
    display.map(x => if (x=="#") 1 else 0).sum
  }
}

