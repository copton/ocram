import se.sics.cooja.Simulation
import se.sics.cooja.GUI
import se.sics.cooja.VisPlugin
import javax.swing._
import javax.swing.text._
import java.awt.{Color, Point, Rectangle, event, Font, GridLayout}

package ch.ethz.inf.vs.ruab {

class CoojaGui(
  val tcdbg: TcodeDebugger,
  val dbginfo: DebugInformation,
  val cooja: VisPlugin
  )
  extends Frontend
{
  import CoojaGui._

  var codes: List[Code] = null
  val green = new DefaultHighlighter.DefaultHighlightPainter(Color.green)
  val red = new DefaultHighlighter.DefaultHighlightPainter(Color.red)

  def start(): Unit = {
    val text = List(dbginfo.tcode, dbginfo.ptcode, dbginfo.ecode)

    val font = Font.decode("Courier 12")
    val areas = text.zipWithIndex map {
      case (txt, idx) => {
        val jta = new JTextArea(txt.text)
        jta.setFont(font)
        jta.setEditable(false)
        jta.addMouseListener(new MouseListener(idx, this))
        jta
      }
    }

    for (area <- areas) area.setHighlighter(new DefaultHighlighter())

    val panes = areas map (new JScrollPane(_))

    cooja.setLayout(new GridLayout(1, panes.length))
    for (pane <- panes) cooja.add(pane)

    codes = for ((text, area) <- text zip areas) yield new Code(text, area)
  }

  def jumpToLine(point: Point) {
    val coord = codes(0).point2coord(point)
    val code = codes(1)
    val hl = code.area.getHighlighter
    val targetRow = dbginfo.trow2ptrow(coord.row)
    hl.removeAllHighlights()
    hl.addHighlight(
      code.text.coord2pos(new Coord(1, targetRow)),
      code.text.eol(targetRow),
      green 
    )
    code.scroll(targetRow)
    code.area.repaint()
  }

  def setBreakpoint(point: Point) {
    val coord = codes(1).point2coord(point)
    Console.println("setBreakpoint: " + coord)
    dbginfo.getLocation(coord) match {
      case None => ()
      case Some(loc) => {
        val hl1 = codes(1).area.getHighlighter
        hl1.removeAllHighlights()
        hl1.addHighlight(loc.start, loc.end, red)
        codes(1).area.repaint()

        val hl2 = codes(2).area.getHighlighter
        hl2.removeAllHighlights()
        hl2.addHighlight(
          codes(2).text.coord2pos(new Coord(1, loc.eloc.row)),
          codes(2).text.eol(loc.eloc.row),
          red
        )
        codes(2).scroll(loc.eloc.row)
        codes(2).area.repaint()
      }
    }
  }


  def stop(): Unit = ()
}

object CoojaGui {
  class MouseListener(val index: Int, val gui: CoojaGui) extends event.MouseListener {
    def mouseClicked(ev: event.MouseEvent): Unit = {
      val point = ev.getPoint()
      val action = (index, ev.getButton())
      action match {
        case (0, event.MouseEvent.BUTTON1) => gui.jumpToLine(point)
        case (1, event.MouseEvent.BUTTON1) => gui.setBreakpoint(point)
        case _ => ()
      }
    }
    def mouseEntered(e: event.MouseEvent): Unit = ()
    def mouseExited(e: event.MouseEvent): Unit = ()
    def mousePressed(e: event.MouseEvent): Unit = ()
    def mouseReleased(e: event.MouseEvent): Unit = ()
  }
  class Code (
    val text: Text,
    val area: JTextArea
  ) {
    private val fm = area.getFontMetrics(area.getFont)
    def point2coord(p: Point): Coord =
      new Coord(p.x / fm.getMaxAdvance + 1, p.y / fm.getHeight + 1)

    def coord2point(coord: Coord): Point =
      new Point(coordCol2pointX(coord.col), coordRow2pointY(coord.row))

    private def coordRow2pointY(row: Int) = (row - 1) * fm.getHeight

    private def coordCol2pointX(col: Int) = (col - 1) * fm.getMaxAdvance 

    def scroll(row: Int): Unit = {
      val viewport = SwingUtilities.getAncestorOfClass(classOf[JViewport], area).asInstanceOf[JViewport]
      val extentHeight = viewport.getExtentSize.height
      val viewHeight = viewport.getViewSize.height
      val center = coordRow2pointY(row)
      val y = math.min(math.max(0, center - (extentHeight / 2)), viewHeight - extentHeight)

      viewport.setViewPosition(new Point(0, y));
    }
  }

}

}
