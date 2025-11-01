import scala.swing.{FlowPanel, *}
import scala.swing.event.*
import javax.swing.{Timer, UIManager}
import scala.math.*
import scala.collection.mutable.Buffer
import java.awt.Polygon
import scala.util.Random

val angular_resolution = 3 // astetta

def make_sweep_polygon(cx: Int, cy: Int, dist: Double, a: Double): Polygon =
  val p = Polygon()
  val ha = (angular_resolution / 2.0).toRadians + 0.01
  val far = 800
  p.addPoint((cx + cos(a-ha) * dist).toInt, (cy + sin(a-ha) * dist).toInt)
  p.addPoint((cx + cos(a-ha) * far).toInt, (cy + sin(a-ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * far).toInt, (cy + sin(a+ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * dist).toInt, (cy + sin(a+ha) * dist).toInt)
  return p

/** sentit pikseleiksi */
def cm2p(x: Double) = (x * 1.0).toInt


/**
 * Custom Panel that renders Lidar observations
 */
class MyCanvas extends Panel:
  preferredSize = new Dimension(700,600)
  focusable = true

  // viimeisimmät mittaukset "rengaspuskurina"
  private val measurements: Array[(Double,Double)] = Array.fill(360)((0,0))
  private var index = 0
  def add_measurement(angleDegrees: Double, distCm: Double) =
    this.measurements(index) = (angleDegrees.toRadians, distCm)
    index = (index + 1) % this.measurements.length

  val debug_bytes = Buffer[Byte]()

  override def paint(g: Graphics2D): Unit =
    super.paint(g)

    // taustaväri
    g.setColor(new Color(10,10,10))
    g.fillRect(0, 0, size.width, size.height)

    // debug display
    g.setColor(new Color(0,100,200))
    g.drawBytes(this.debug_bytes.toArray, 0, this.debug_bytes.length, 10,20)

    val (cx,cy) = (size.width / 2, size.height / 2)
    val t = System.currentTimeMillis() / 1000.0

    // grid (metrin ruudut)
    g.setColor(new Color(30,30,30))
    for x <- (0 to 10) do g.drawLine(x * cm2p(100), 0, x * cm2p(100), size.height)
    for y <- (0 to 10) do g.drawLine(0, y * cm2p(100), size.width, y * cm2p(100))

    // havainnot
    g.setColor(new Color(100,100,100))
    for (angle,dist) <- this.measurements do if dist > 10 then
      UI.visChooser.selection.index match {
        case 0 =>
          g.fillPolygon(make_sweep_polygon(cx, cy, cm2p(dist), angle))
        case 1 =>
          g.fillOval((cx + cos(angle) * cm2p(dist)).toInt - 5,
                     (cy + sin(angle) * cm2p(dist)).toInt - 5, 10,10)
      }

    // hub
    g.setColor(new Color(100,100,100))
    val (hw,hh) = (cm2p(11) / 2, cm2p(18) / 2)
    g.drawRoundRect(cx-hw,cy-hh, 2*hw,2*hh, 20,20)
    // scan line
    g.setColor(new Color(0,150,0))
    g.drawLine(cx,cy, (cx + cos(t)*150).toInt, (cy + sin(t)*150).toInt)
  end paint

  listenTo(keys)
  reactions += {
    case e: KeyPressed if e.key == Key.Escape =>
      UI.quit()
  }

end MyCanvas


/**
 * Main UI object
 */
object UI extends SimpleSwingApplication:
  // tyylikkäämpi teema
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  // UI
  val panel = BoxPanel(Orientation.Vertical)

  // tämä hoitaa grafiikan
  val canvas = new MyCanvas()
  panel.contents += canvas

  val info = Label("No connection")
  val help = Label("Use WASD or Arrow Keys to control")
  val visChooser = ComboBox(Vector("Polygons", "Dots"))
  visChooser.focusable = false
  val panel2 = FlowPanel(info, help, visChooser)
  panel.contents += panel2

  // avataan yhteys robottiin
  Comm.open_serial()

  // timer that triggers repaint 30 times per second
  val timer = new Timer(33, Swing.ActionListener { _ => this.onTick() })
  timer.start()


  // pääikkuna (anonyymi luokka johdettu MainFrame:sta)
  def top = new MainFrame():
    title = "Remote Controller"
    contents = panel
    minimumSize = new Dimension(500,400)
    centerOnScreen()
    listenTo(this)
    reactions += {
      case e: WindowClosing =>
        quit()
    }


  // callback 30 times per second
  private def onTick() =
    Comm.currentSerial match {
      case Some(port) if port.isOpen =>
        info.foreground = new Color(0,200,0)
        info.text = port.getPortDescription + " connected"
      case opt =>
        info.foreground = new Color(250,0,0)
        info.text = "No connection " + opt.map(port => s"(error code ${port.getLastErrorCode})").getOrElse("")
    }
    if Comm.data_available then
      canvas.debug_bytes ++= Comm.read_data(1)
    canvas.add_measurement(Random.between(0,360), Random.between(100,500))
    canvas.repaint()
  end onTick


  // custom cleanup actions
  override def quit(): Unit =
    println("Quitting...")
    Comm.close_serial()
    super.quit()

end UI

