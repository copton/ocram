import se.sics.cooja.Simulation
import se.sics.cooja.GUI
import se.sics.cooja.VisPlugin
import javax.swing._
import javax.swing.text._
import java.awt.{Color, Point, Rectangle, event, Font, GridLayout}

package ch.ethz.inf.vs.ruab {

class CoojaGui(
  val tcdbg: TcodeDebugger,
  val tcodefile: String,
  val ecodefile: String,
  val dbginfo: DebugInformation,
  val cooja: VisPlugin
  )
  extends Frontend
{
  import CoojaGui._

  var codes: List[Code] = null
  val debuginfo = dbginfo.load()
  val painter = new DefaultHighlighter.DefaultHighlightPainter(Color.green)

  def start(): Unit = {
    val text = List(
      Utils.readFileVerify(tcodefile, debuginfo.checksum("tcode")),
      debuginfo.preprocessed,
      Utils.readFileVerify(ecodefile, debuginfo.checksum("ecode"))
    )

    val mappers = text map (new TextPositionMapper(_))

    val font = Font.decode("Courier 12")
    val areas = text.zipWithIndex map {
      case (txt, idx) => {
        val jta = new JTextArea(txt)
        jta.setFont(font)
        jta.setEditable(false)
        jta.addMouseListener(new MouseListener(idx, this))
        jta
      }
    }

    for (area <- areas) area.setHighlighter(new DefaultHighlighter())


    
/*
    for (loc <- debuginfo.locmap) {
      val coord = (loc.tloc.row, loc.tloc.col)
      val start = mapper(0).coord2pos(coord)
      val end = start + loc.tloc.len
      hl.addHighlight(start, end, painter)
    }
*/

    val panes = areas map (new JScrollPane(_))

    cooja.setLayout(new GridLayout(1, panes.length))
    for (pane <- panes) cooja.add(pane)

    codes = for ((mapper, area) <- mappers zip areas) yield new Code(mapper, area)
  }

  def mouseClicked(index: Int, ev: event.MouseEvent): Unit = {
    val code = codes(index)
    val fm = code.area.getFontMetrics(code.area.getFont)
    val col: Int = ev.getX / fm.getMaxAdvance + 1
    val row: Int = ev.getY / fm.getHeight + 1

    Console.println("idx: " + index + ", row: " + row + ", col: " + col)

    index match {
      case 0 => jumpToLine(row)
      case 1 => setBreakpoint(row, col)
      case _ => ()
    }
  }

  def jumpToLine(row: Int) {
    val code = codes(1)
    val area = code.area
    val mapper = code.mapper
    val viewport = SwingUtilities.getAncestorOfClass(classOf[JViewport], area).asInstanceOf[JViewport]
    val mapping = debuginfo.prepmap.takeWhile(_._1 < row).last
    val targetRow = mapping._2 + (row - mapping._1) + 1
    val hl = area.getHighlighter
    val coord = new Coord(1, targetRow)

    hl.removeAllHighlights()
    hl.addHighlight(mapper.coord2pos(coord), mapper.eol(targetRow), painter)

    val extentHeight = viewport.getExtentSize.height;
	  val viewHeight = viewport.getViewSize.height;
    val center = code.coord2point(coord)
    val y = math.min(math.max(0, center.y - (extentHeight / 2)), viewHeight - extentHeight)

    viewport.setViewPosition(new Point(0, y));

    area.repaint()
  }

  def setBreakpoint(row: Int, col: Int) { }

  def stop(): Unit = ()
}

object CoojaGui {
  class MouseListener(val index: Int, val gui: CoojaGui) extends event.MouseListener {
    def mouseClicked(ev: event.MouseEvent): Unit = gui.mouseClicked(index, ev)
    def mouseEntered(e: event.MouseEvent): Unit = ()
    def mouseExited(e: event.MouseEvent): Unit = ()
    def mousePressed(e: event.MouseEvent): Unit = ()
    def mouseReleased(e: event.MouseEvent): Unit = ()
  }
  case class Coord(val col: Int, val row: Int)
  class Code (
    val mapper: TextPositionMapper, 
    val area: JTextArea
  ) {
    private val fm = area.getFontMetrics(area.getFont)
    def point2coord(p: Point): Coord =
      new Coord(p.x / fm.getMaxAdvance + 1, p.y / fm.getHeight + 1)
    def coord2point(coord: Coord): Point =
      new Point((coord.col - 1) * fm.getMaxAdvance, (coord.row - 1) * fm.getHeight)
  }

  class TextPositionMapper(val text: String) {
    val lineLength = (text.split("\n") map (_.length + 1)).toList
    val lineSum = lineLength.foldLeft((0, List[Int]()))((sum: (Int, List[Int]), elem: Int) =>
      (sum._1 + elem, sum._1 :: sum._2))._2.reverse

    def coord2pos(coord: Coord): Int = lineSum(coord.row - 1) + coord.col - 1

    def eol(row: Int): Int = lineSum(row)

    def pos2coord(pos: Int): Coord = {
      val rows = lineSum.takeWhile(_ < pos)
      new Coord(rows.length, pos - rows.last + 1)
    }
  }
}

}
