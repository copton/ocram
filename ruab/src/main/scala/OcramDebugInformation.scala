package ch.ethz.inf.vs.ruab {

import net.liftweb.json._

private case class File(
  file: String,
  checksum: String
)

private case class Info(
  tcode: File,
  ptcode: File,
  ecode: File,
  prepmap: List[List[Int]],
  locmap: List[List[Int]],
  varmap: List[Int]
)


class OcramDebugInformation(val debugfile: String)
  extends DebugInformation
{
  var tcode_ : String = null
  var ptcode_ : String = null
  var ecode_ : String = null
  var prepmap: List[(Int, Int)] = Nil
  var locmap: List[Location] = Nil

  load()

  private def load(): Unit = {
    val jsonstr = Utils.readFile(debugfile)
    val jsonobj = JsonParser.parse(jsonstr)
    implicit val formats = DefaultFormats
    val inf = jsonobj.extract[Info]
  
    tcode_ = Utils.readFileVerify(inf.tcode.file, inf.tcode.checksum)
    ptcode_ = Utils.readFileVerify(inf.ptcode.file, inf.ptcode.checksum)
    ecode_ = Utils.readFileVerify(inf.ecode.file, inf.ecode.checksum)
    
    prepmap = for (elems <- inf.prepmap) yield (elems(0), elems(1))
    locmap = for (
        elems <- inf.locmap;
        val tid = if (elems.length == 5) None else Some(elems(5));
        val loc = new Location(
          new TLocation(elems(0), elems(1), elems(2)),
          new ELocation(elems(3), elems(4), tid)
        )
      ) yield loc
  }

  override def tcode(): String = tcode_
  override def ptcode(): String = ptcode_
  override def ecode(): String = ecode_

  override def trow2ptrow(row: Int): Int = {
    val mapping = prepmap.takeWhile(_._1 < row).last
    mapping._2 + (row - mapping._1) + 1
  }
  
}

}
