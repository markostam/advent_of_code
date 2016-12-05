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

  def bruteForceCrack1 (doorID: String) = {
    // lazily evaluate a brute force attack
    lazy val passwordCrack = Stream.from(0).
      map(doorID + _).
      map(x => hash(x)).
      filter(x => (x(0)=="0" & x(1)=="0" & hex2int(x(2)) < 16)).
      map(_(2))
    (0 to 7).map(x => passwordCrack(x)).mkString("")
  }

  def bruteForceCrack3 (doorID: String) = {
    var seen = Set[Any]()
    def test_unique (tup : (Any,Any)) : Boolean = {
      if (!seen.contains(tup._1)) {
        seen += tup._1
        true
      }
      else {false}
    }
    // lazily evaluate a brute force attack
    lazy val passwordCheck = Stream.from(0).
      map(doorID + _).
      map(x => hash(x)).
      filter(x => (x(0)=="0" & x(1)=="0" & hex2int(x(2)) < 8)).
      map(x => (x(2),x(3))).filter(x => test_unique(x))
    //lazy val passwordCrack = unique_new(passwordCheck)
    (0 to 7).map(x => passwordCheck(x))//.mkString("")
  }



  def main(args:Array[String]):Unit = {
    // first part
    val doorID = args(0)
    val password1 = bruteForceCrack1(doorID)
    println("First part: " + password1)
    // 2nd part
    val codes = getRooms(args(0)).map(x => (x(0).split("-"),x(2)))
    val decoded = codes.map(x => ((x._1.map(y => y.
                  map(z => decodeChar(z,x._2.toInt)))).reduce(_+" "+_), x._2)).toMap
    val queryRoom = "northpole object storage"
    println("Second part: " + decoded(queryRoom))
  }
}