import breeze.linalg._

var display = new DenseMatrix[String](6,50, Array.fill(300)("."))
//val input = scala.io.Source.fromFile(args(0)).getLines.toList
val input = scala.io.Source.fromFile("/Users/markostamenovic/code/adventOfCode/Day8/input.txt").getLines.toList

val prob1 = input.filter(_.split(" ")(0) == "rect").
  map(x => x.split(" ")).map(_(1).split("x")).
  map(x => x(1).toInt*x(0).toInt).reduce(_+_)
var iter=1
for (line <- input) {
    val lineSplit = line.split(" ")
    lineSplit(0) match {
      case "rect" => {
            // if (lineSplit(0) == "rect")
            println(line)
            val colRow = lineSplit(1).split("x").map(_.toInt)
            println(colRow)
            display(0 to colRow(1)-1,0 to colRow(0)-1) := "#"
            println(display)
        }
      case "rotate" => {
        lineSplit(1) match {
          case "row" => {
              val row = lineSplit(2).split("=")(1).asDigit
              val shift = lineSplit.last.toInt
              println(line)
              println(row)
              println(shift)
              display(row,::).t := roll(display(row,::).t,shift)
              println(display)
          }
          case "column" => {
              val column = lineSplit(2).split("=")(1).asDigit
              val shift = lineSplit.last.toInt
              println(line)
              println(column)
              println(shift)
              display(::,column) := roll(display(::,column),shift)
              println(display)
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
  println(iter)
  iter+=1
}

display.map(x => if (x=="#") 1 else 0).sum
