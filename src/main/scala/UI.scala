import scala.swing.{FlowPanel, *}
import scala.swing.event.*
import javax.swing.{Timer, UIManager}
import javax.imageio.ImageIO
import java.awt.{Font, Image, Polygon}
import java.io.File
import scala.math.*
import scala.util.Random
import scala.collection.mutable.Buffer

val angular_resolution = 2 // astetta

def make_sweep_polygon(cx: Int, cy: Int, near: Double, a: Double): Polygon =
  val p = Polygon()
  val ha = (angular_resolution / 2.0).toRadians + 0.001
  val far = 800
  p.addPoint((cx + cos(a-ha) * near).toInt, (cy + sin(a-ha) * near).toInt)
  p.addPoint((cx + cos(a-ha) * far).toInt, (cy + sin(a-ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * far).toInt, (cy + sin(a+ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * near).toInt, (cy + sin(a+ha) * near).toInt)
  /*return*/ p

def seqMatch(a: Seq[Int], b: Seq[Int]): Boolean =
  a.length >= b.length &&
  a.zipWithIndex.forall( (x,i) => (x == b(i)) )

/** sentit pikseleiksi */
def cm2p(x: Double) = (x * 1.0).toInt


/**
 * Kustomoitu Panel joka renderöi Lidarin havainnot
 */
class MyCanvas extends Panel:
  preferredSize = new Dimension(700,600)
  focusable = true
  val carImg = ImageIO.read(new File("car.png"))

  // viimeisimmät mittaukset "rengaspuskurina"
  private val measurements: Array[(Double,Double)] = Array.fill(360)((0,0))
  private var index = 0
  def add_measurement(angleDegrees: Double, distCm: Double) =
    this.measurements(index) = (angleDegrees.toRadians, distCm)
    index = (index + 1) % this.measurements.length

  // tämä piirtää grafiikan
  override def paint(g: Graphics2D): Unit =
    super.paint(g)

    // taustaväri
    g.setColor(new Color(10,10,10))
    g.fillRect(0, 0, size.width, size.height)

    val (cx,cy) = (size.width / 2, size.height / 2)
    //val t = System.currentTimeMillis() / 1000.0
    val t = this.measurements(max(0,this.index-1))._1

    // grid (metrin ruudut)
    g.setColor(new Color(30,30,30))
    for x <- (-10 to 10) do g.drawLine(cx + x * cm2p(100), 0, cx + x * cm2p(100), size.height)
    for y <- (-10 to 10) do g.drawLine(0, cy + y * cm2p(100), size.width, cy + y * cm2p(100))

    // havainnot
    g.setColor(new Color(100,100,100))
    for (angle,dist) <- this.measurements do if dist > 0 then
      UI.visChooser.selection.index match {
        case 0 =>
          g.fillPolygon(make_sweep_polygon(cx, cy, cm2p(dist), angle))
        case 1 =>
          g.fillOval((cx + cos(angle) * cm2p(dist)).toInt - 5,
                     (cy + sin(angle) * cm2p(dist)).toInt - 5, 10,10)
      }

    // robotin kuva
    g.setColor(new Color(100,100,100))
    val (hw,hh) = (cm2p(11) / 2, cm2p(18) / 2)
    //g.drawRoundRect(cx-hw,cy-hh, 2*hw,2*hh, 10,10)
    g.drawImage(carImg, cx-hw,cy-hh, hw*2,hh*2, null)

    // scan line
    g.setColor(new Color(0,150,0))
    g.drawLine(cx,cy, (cx + cos(t)*150).toInt, (cy + sin(t)*150).toInt)
  end paint

  // näppäimistötapahtumat
  listenTo(keys)
  reactions += {
    case e: KeyPressed =>
      e.key match {
        case Key.Escape =>
          // lopeta sovellus
          UI.quit()
        case Key.Space =>
          Comm.write_data(Comm.START_SCAN)
        case _ =>
          ()
      }
  }

end MyCanvas


/**
 * Main UI object
 */
object UI extends SimpleSwingApplication:
  // tyylikkäämpi teema
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  //UIManager.put("Label.font", new Font("Arial", Font.PLAIN, 14))

  // UI
  val panel = BoxPanel(Orientation.Vertical)

  // tämä hoitaa grafiikan
  val canvas = new MyCanvas()
  panel.contents += canvas

  // alapaneeli
  val info = Label("No connection")
  val help = Label("Use WASD or Arrow Keys to control")
  val visChooser = ComboBox(Vector("Polygons", "Dots"))
  visChooser.focusable = false
  val panel2 = FlowPanel(info, help, visChooser)
  panel.contents += panel2

  // debug serial monitor
  val serialText = new TextArea():
    editable = false
    wordWrap = true
    lineWrap = true
    background = new Color(50,50,50)
    foreground = new Color(0,255,0)
  val serialMonitor = new Frame():
    contents = new ScrollPane(serialText)
    size = new Dimension(500,300)
    title = "Serial monitor"

  // avataan yhteys robottiin
  Comm.open_serial()

  // Timer kutsuu onTick() noin 30 kertaa sekunnissa
  val tickTimer = new Timer(33, Swing.ActionListener { _ => this.onTick() })
  tickTimer.start()

  var tempCounter = 0
  serialMonitor.visible = true
  var dataCache = Buffer[Int]()
  var scanStarted = false

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


  private def onTick() =
    // connection status
    Comm.currentSerial match {
      case Some(port) if port.isOpen =>
        info.foreground = new Color(0,200,0)
        info.text = port.getPortDescription + " connected"
      case opt =>
        info.foreground = new Color(250,0,0)
        info.text = "No connection " + opt.map(port => s"(error code ${port.getLastErrorCode})").getOrElse("")
    }
    // tulosta vastaanotetut tavut
    if Comm.data_available then
      val received_data = Comm.read_data(100)
      if !serialText.hasFocus then
        serialText.text += received_data.map(String.format("0x%X",_)).mkString(" ") + " "
      // lue ja tulkitse saapuva data
      dataCache ++= received_data
      if !scanStarted then
        if seqMatch(dataCache.take(7).toVector, Comm.START_SCAN_RESPONSE) then
          println("Scan started")
          dataCache = dataCache.drop(7)
          scanStarted = true
      if scanStarted then
        // lue skannauspaketteja niin monta kuin löytyy
        while scanStarted && dataCache.length >= 5 do
          val scanPacket = dataCache.take(5)
          dataCache = dataCache.drop(5)
          val angledeg: Int = (scanPacket(2)<<7 | scanPacket(1)>>1) / 64
          val distmm: Int = (scanPacket(4)<<8 | scanPacket(3)) / 4
          canvas.add_measurement(angledeg, distmm / 10.0)

          //if !serialText.hasFocus then
          //  serialText.text += s"(${angledeg} deg ${distmm} mm)\n"
          //println(s"scan ${angledeg}deg ${distmm}mm")

    //canvas.add_measurement(tempCounter, Random.between(150,400))
    //tempCounter = (tempCounter+angular_resolution) % 360

    canvas.repaint()
  end onTick


  // sulje yhteys hallitusti
  override def quit(): Unit =
    println("Quitting...")
    Comm.write_data(Comm.STOP)
    Comm.close_serial()
    super.quit()

end UI

