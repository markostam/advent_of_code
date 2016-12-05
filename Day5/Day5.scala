import java.security.MessageDigest

def md5(s: String) = {
  MessageDigest.getInstance("MD5").digest(s.getBytes)
}

val doorID = "uqwqemis"

def hash(s: String) : Array[String] = {
  md5(s.toString).map(x => if (x*(-1) > 0) (x+256).
  toHexString else x.toHexString)
}

val thing = 1 to 100000 toArray

lazy val thing2 = (1 to 10000).toArray.map(_ + doorID).map(x => hash(x)).
  filter(x => (x(0)=="0" & x(1)=="0" & Integer.parseInt(x(2), 16) < 10)).map(_(2))

val thing3 = thing2.map(x => x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7)+x(8))

var i = 1
var output = ""

def decode_hash (doorID: String, i:Int, output:String) {
  val test = hash(i + doorID)
  if (output.length < 8) {
    if (test(0) == "0" & test(1) == "0" & test(2).toInt < 10) {
      output.concat(test(2))
      i = i + 1
      decode_hash(doorID, i, output)
    }
  }
  output
}