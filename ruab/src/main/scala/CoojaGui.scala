import se.sics.cooja.Simulation
import se.sics.cooja.GUI
import se.sics.cooja.VisPlugin
import javax.swing._
import javax.swing.text._
import java.awt.Color

package ch.ethz.inf.vs.ruab {

class CoojaGui(
  tcdbg: TcodeDebugger,
  tcodefile: String,
  ecodefile: String,
  dbginfo: DebugInformation,
  cooja: VisPlugin
  )
  extends Frontend
{
  def start(): Unit = {
    val debuginfo = dbginfo.load()
    val params = List((tcodefile, "tcode"), (ecodefile, "ecode"))

    val text = params map {case (name, idx) => Utils.readFileVerify(
          name,
          debuginfo.checksum(idx)
        ).getOrElse(
         throw new RuntimeException("checksum for %s failed" format name)
        )
      }

    val mapper = text map (new TextPositionMapper(_))

    val areas = text map ((txt) => {
      val jta = new JTextArea(txt)
      jta.setEditable(false)
      jta
    })

    val hl = new DefaultHighlighter()
    areas(0).setHighlighter(hl)
    val painter = new DefaultHighlighter.DefaultHighlightPainter(Color.green)
    for (loc <- debuginfo.locmap) {
      val coord = (loc.tloc.row, loc.tloc.col)
      val start = mapper(0).coord2pos(coord)
      val end = start + loc.tloc.len
      hl.addHighlight(start, end, painter)
    }

    val panes = areas map (new JScrollPane(_))


    cooja.add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, panes(0), panes(1)))
  }

  def stop(): Unit = ()
}

class TextPositionMapper(val text: String) {
  val lineLength = (text.split("\n") map (_.length + 1)).toList
  val lineSum = lineLength.foldLeft((0, List[Int]()))((sum: (Int, List[Int]), elem: Int) =>
    (sum._1 + elem, sum._1 :: sum._2))._2.reverse

  def coord2pos(coord : (Int, Int)): Int = lineSum(coord._1 - 1) + coord._2 - 1

  def pos2coord(pos: Int): (Int, Int) = {
    val rows = lineSum.takeWhile(_<pos)
    (rows.length, pos - rows.last + 1)
  }
}

}
