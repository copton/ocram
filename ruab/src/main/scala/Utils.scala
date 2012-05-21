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

  def readFileVerify(filename: String, sum: String): Option[String]= {
    val source = io.Source.fromFile(filename)
    val lines = source.mkString
    val md = MessageDigest.getInstance("MD5")
    md.update(lines.getBytes)
    val sumIs = getHex(md.digest())
    source.close() 
    if (sumIs == sum) Some(lines) else None
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

  def foldIncludes(code: String): String = {
    val Pattern = """# (\d+) "([^"]+)".*""".r
    val Internal = """<[^>]*>""".r
    var mainFile: String = null
    var mainScope = false
 
    val result: ListBuffer[String] = new ListBuffer()
    for (line <- code.split("\n")) {
      line match {
        case Pattern(row, includeFile) =>
          if (mainFile == null) {
            mainFile = includeFile
            mainScope = true
          } else {
            if (mainScope) {
              mainScope = false
              includeFile match {
                case Internal() => null
                case _ => result += ("#include \"%s\"" format includeFile)
              }
            } else {
              if (includeFile == mainFile) {
                mainScope = true
              }
            }
          }
        case _ => if (mainScope == true) result += line
      }
    }
    result.mkString("", "\n", "") 
  }
}

}
