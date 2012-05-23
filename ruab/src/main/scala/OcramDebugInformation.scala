package ch.ethz.inf.vs.ruab {

import net.liftweb.json._

private case class Info(
  checksum: List[String],
  prepmap: List[List[Int]],
  locmap: List[List[Int]],
  varmap: List[Int],
  preprocessed: String
)


class OcramDebugInformation(val debugfile: String)
  extends DebugInformation
{
  var info: Information = null

  override def load(): Information = {
    if (info == null) {
      val jsonstr = Utils.readFile(debugfile)
      val jsonobj = JsonParser.parse(jsonstr)
      implicit val formats = DefaultFormats
      val inf = jsonobj.extract[Info]
      val checksum = Map("tcode" -> inf.checksum(0), "ecode" -> inf.checksum(1))
      val prepmap = for (elems <- inf.prepmap) yield (elems(0), elems(1))
      
      val locmap = for (
          elems <- inf.locmap;
          val tid = if (elems.length == 5) None else Some(elems(5));
          val loc = new Location(
            new TLocation(elems(0), elems(1), elems(2)),
            new ELocation(elems(3), elems(4), tid)
          )
        ) yield loc

      info = new Information(
        checksum,
        prepmap,
        locmap,
        Nil,
        inf.preprocessed
      )
    }
    info
  }
  
}

}
