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

  def bruteForceCrack2 (doorID: String) = {
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
      map(x => if (x(3).length == 1) (x(2),"0") else (x(2),x(3)(0))).
      filter(x => test_unique(x))
    val unsorted = (0 to 7).map(x => passwordCheck(x))
    unsorted.sortBy(_._1).map(_._2).mkString("")
  }

  def main(args:Array[String]):Unit = {
    // first part
    val doorID = args(0)
    val password1 = bruteForceCrack1(doorID)
    println("First part: " + password1)
    // 2nd part
    val doorID2 = args(1)
    val password1 = bruteForceCrack1(doorID)
    println("First part: " + password1)

  }
}