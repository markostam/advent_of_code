import java.security.MessageDigest

object Advent5 {

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  def hash(s: String) : Array[String] = {
    md5(s.toString).map(x => if (x < 0) 
      (x+256).toHexString else x.toHexString)
  }

  def hex2int (hex: String): Int = {
    Integer.parseInt(hex, 16)
  }

  def bruteForceCrack (doorID: String) = {
    lazy val passwordCrack = (1 to 100000000).toStream.map(doorID + _).map(x => hash(x)).
      filter(x => (x(0)=="0" & x(1)=="0" & hex2int(x(2)) < 16)).map(_(2))
    password (0 to 7).map(x => passwordCrack(x))
  }

  def main(args:Array[String]):Unit = {
    // first part
    val doorID = args(0)
    val rooms = getRooms(args(0)).map(x => Array(x(0).replace("-",""),x(1),x(2)))
    val pairs = rooms.map(x => (x(1),top5Words(countWords(x(0))),x(2).toInt)).
                map(x => if (x._1 == x._2) x._3 else 0).reduce(_+_)
    println("First part: " + pairs)
    // 2nd part
    val codes = getRooms(args(0)).map(x => (x(0).split("-"),x(2)))
    val decoded = codes.map(x => ((x._1.map(y => y.
                  map(z => decodeChar(z,x._2.toInt)))).reduce(_+" "+_), x._2)).toMap
    val queryRoom = "northpole object storage"
    println("Second part: " + decoded(queryRoom))
  }
}