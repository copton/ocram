package ch.ethz.inf.vs.ruab {

import java.security.MessageDigest
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

object Utils {
  def readFile(filename: String): String = {
    val source = io.Source.fromFile(filename)
    val lines = source.mkString
    source.close()
    lines
  }

  def readFileVerify(filename: String, sum: String): String = {
    val source = io.Source.fromFile(filename)
    val lines = source.mkString
    val md = MessageDigest.getInstance("MD5")
    md.update(lines.getBytes)
    val sumIs = getHex(md.digest())
    source.close() 
    if (sumIs == sum) lines else throw new RuntimeException("checksum of %s failed" format filename)
  }

  def getHex(raw: Array[Byte]): String = {
    if ( raw == null ) {
      return null;
    }
    val hex = new StringBuilder(2 * raw.length);
    for (b <- raw ) {
      hex.append(HEXES.charAt((b & 0xF0) >> 4))
        .append(HEXES.charAt((b & 0x0F)));
    }
    hex.toString
  }

  private final val HEXES = "0123456789abcdef";
}

}
