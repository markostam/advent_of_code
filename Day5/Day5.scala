import java.security.MessageDigest

def md5(s: String) = {
  MessageDigest.getInstance("MD5").digest(s.getBytes)
}

val DoorID = "uqwqemis"

val thing = md5(hash).map(_.toHexString)
