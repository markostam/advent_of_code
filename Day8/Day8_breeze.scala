import breeze.linalg._

var display = new DenseMatrix[String](6,50, Array.fill(300)(" "))
val input = scala.io.Source.fromFile(args(0)).getLines.toList

val prob1 = input.filter(_.split(" ")(0) == "rect").
  map(x => x.split(" ")).map(_(1).split("x")).
  map(x => x(1).toInt*x(0).toInt).reduce(_+_)
var iter=1
for (line <- input) {
    val lineSplit = line.split(" ")
    lineSplit(0) match {
      case "rect" => {
            // if (lineSplit(0) == "rect")
            val colRow = lineSplit(1).split("x").map(_.toInt)
            display(0 to colRow(1)-1,0 to colRow(0)-1) := "#"
        }
      case "rotate" => {
        lineSplit(1) match {
          case "row" => {
              val row = lineSplit(2).split("=")(1).toInt
              val shift = lineSplit.last.toInt
              display(row,::).t := roll(display(row,::).t,shift)
          }
          case "column" => {
              val column = lineSplit(2).split("=")(1).toInt
              val shift = lineSplit.last.toInt
              display(::,column) := roll(display(::,column),shift)
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

val prob1 = display.map(x => if (x=="#") 1 else 0).sum
println("Problem 1: " + prob1)
println("Problem 2:")
println(display(0 to 5, 0 to 24))
println(display(0 to 5, 25 to 49))