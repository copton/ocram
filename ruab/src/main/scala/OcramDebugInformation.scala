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
  var tcode_ : Text = null
  var ptcode_ : Text = null
  var ecode_ : Text = null
  var prepmap: List[(Int, Int)] = Nil
  var locmap: List[Location] = Nil

  load()

  class TextMapper (override val text: String) extends Text(text) {
    val lineLength = (text.split("\n") map (_.length + 1)).toList
    val lineSum = lineLength.foldLeft((0, List[Int]()))((sum: (Int, List[Int]), elem: Int) =>
      (sum._1 + elem, sum._1 :: sum._2))._2.reverse

    override def coord2pos(coord: Coord): Int = lineSum(coord.row - 1) + coord.col - 1

    override def eol(row: Int): Int = lineSum(row)

    override def pos2coord(pos: Int): Coord = {
      val rows = lineSum.takeWhile(_ < pos)
      new Coord(rows.length, pos - rows.last + 1)
    }
  }

  private def load(): Unit = {
    val jsonstr = Utils.readFile(debugfile)
    val jsonobj = JsonParser.parse(jsonstr)
    implicit val formats = DefaultFormats
    val inf = jsonobj.extract[Info]
  
    tcode_ = new TextMapper(Utils.readFileVerify(inf.tcode.file, inf.tcode.checksum))
    ptcode_ = new TextMapper(Utils.readFileVerify(inf.ptcode.file, inf.ptcode.checksum))
    ecode_ = new TextMapper(Utils.readFileVerify(inf.ecode.file, inf.ecode.checksum))
    
    prepmap = for (elems <- inf.prepmap) yield (elems(0), elems(1))
    locmap = for (
        elems <- inf.locmap;
        val tid = if (elems.length == 5) None else Some(elems(5));
        val (trow, tcol, tlen, erow, rcol) = (elems(0),elems(1), elems(2), elems(3), elems(4));
        val start = ptcode.coord2pos(new Coord(tcol, trow2ptrow(trow)));
        val loc = new Location(
          start, 
          start + tlen,
          new TLocation(trow, tcol, tlen),
          new ELocation(erow, rcol, tid)
        )
      ) yield loc
    locmap = locmap sortWith ((l1,l2) => l1.start < l2.start || (l1.start == l2.start && l1.end> l2.end))
  }

  override def tcode(): Text = tcode_
  override def ptcode(): Text = ptcode_
  override def ecode(): Text = ecode_

  override def trow2ptrow(row: Int): Int = {
    val mapping = prepmap.takeWhile(_._1 < row).last
    mapping._2 + (row - mapping._1) + 1
  }

  override def getLocation(coord: Coord): Option[Location] = {
    val pos = ptcode.coord2pos(coord)
    locmap find (loc => {
      Console.println(loc)
      loc.start <= pos && loc.end >= pos
    })
  }
}

}
