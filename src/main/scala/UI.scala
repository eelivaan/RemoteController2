import scala.swing.{FlowPanel, *}
import scala.swing.event.*
import javax.swing.{Timer, UIManager}
import javax.imageio.ImageIO
import java.awt.{Font, Image, Polygon}
import java.io.File
import scala.math.*
import scala.collection.mutable.Queue

val angular_resolution: Double = 1.0 // astetta

def make_sweep_polygon(cx: Int, cy: Int, near: Double, a: Double): Polygon =
  val p = Polygon()
  val ha = (angular_resolution / 2.0).toRadians + 0.001
  val far = 800
  p.addPoint((cx + cos(a-ha) * near).toInt, (cy + sin(a-ha) * near).toInt)
  p.addPoint((cx + cos(a-ha) * far).toInt, (cy + sin(a-ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * far).toInt, (cy + sin(a+ha) * far).toInt)
  p.addPoint((cx + cos(a+ha) * near).toInt, (cy + sin(a+ha) * near).toInt)
  /*return*/ p

val controlMap = Map[Key.Value, Char](
  Key.W -> 'w', Key.Up -> 'w',
  Key.S -> 's', Key.Down -> 's',
  Key.A -> 'a', Key.Left -> 'a',
  Key.D -> 'd', Key.Right -> 'd',
)

/**
 * Kustomoitu Panel joka renderöi Lidarin havainnot
 */
class MyCanvas extends Panel:
  preferredSize = new Dimension(700,600)
  focusable = true
  val carImg = ImageIO.read(new File("car.png"))
  var mapScale = 1.0

  // viimeisimmät 360 mittausta
  val measurements = Queue[(Double,Double)]()
  def add_measurement(angleDegrees: Double, distCm: Double) =
    this.measurements.enqueue(((angleDegrees - 90.0).toRadians, distCm))
    if this.measurements.length > 360 then
      this.measurements.dequeue()

  /** sentit pikseleiksi */
  def cm2p(x: Double) = (x * mapScale).toInt

  //noinspection IllegalNull
  override def paint(g: Graphics2D): Unit =
    super.paint(g)

    // taustaväri
    g.setColor(new Color(10,10,10))
    g.fillRect(0, 0, size.width, size.height)

    val (cx,cy) = (size.width / 2, size.height / 2)

    // grid (metrin ruudut)
    g.setColor(new Color(30,30,30))
    for x <- (-10 to 10) do g.drawLine(cx + x * cm2p(100), 0, cx + x * cm2p(100), size.height)
    for y <- (-10 to 10) do g.drawLine(0, cy + y * cm2p(100), size.width, cy + y * cm2p(100))

    // havainnot
    g.setColor(new Color(100,100,100))
    for (angle,dist) <- this.measurements do if dist > 0 then
      UI.visChooser.selection.index match {
        case 0 =>
          g.drawOval((cx + cos(angle) * cm2p(dist)).toInt - 5,
                     (cy + sin(angle) * cm2p(dist)).toInt - 5, 5,5)
        case 1 =>
          g.fillPolygon(make_sweep_polygon(cx, cy, cm2p(dist), angle))
      }

    // robotin kuva
    g.setColor(new Color(100,100,100))
    val (hw,hh) = (cm2p(11) / 2, cm2p(18) / 2)
    //g.drawRoundRect(cx-hw,cy-hh, 2*hw,2*hh, 10,10)
    g.drawImage(carImg, cx-hw,cy-hh, hw*2,hh*2, null)

    // scan line
    if this.measurements.nonEmpty then
      val t = System.currentTimeMillis() / 1000.0
      //val t = this.measurements.last._1
      g.setColor(new Color(0,150,0))
      g.drawLine(cx,cy, (cx + cos(t)*200).toInt, (cy + sin(t)*200).toInt)
  end paint

  // näppäimistötapahtumat
  listenTo(keys, mouse.wheel)
  reactions += {
    case KeyPressed(_, key, _,_) =>
      // ajokomento
      if VirtualCar.curDriveCommand.isEmpty then
        controlMap.get(key).foreach(char =>
          Comm.write_data(Vector(char.toInt))
          VirtualCar.curDriveCommand = Some(char)
        )

    case KeyReleased(_, key, _,_) =>
      key match {
        case Key.Escape =>
          // lopeta sovellus
          UI.quit()
        case Key.Space if !VirtualCar.scanning =>
          //Comm.write_data(Comm.START_SCAN)
        case anyKey =>
          // ajon pysäytys
          controlMap.get(anyKey).foreach(char =>
            Comm.write_data(Vector('e'.toInt))
            VirtualCar.curDriveCommand = None
          )
      }

    case MouseWheelMoved(_,_,_, value) =>
      this.mapScale *= 1.0 + value*0.05
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
  val visChooser = ComboBox(Vector("Dots", "Polygons"))
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
    size = new Dimension(500,600)
    title = "Serial monitor"

  // avataan yhteys robottiin
  Comm.open_serial()

  // Timer kutsuu onTick() noin 30 kertaa sekunnissa
  val tickTimer = new Timer(33, Swing.ActionListener { _ => this.onTick() })
  tickTimer.start()

  var tempCounter = 0
  serialMonitor.visible = false
  var bufferedSerialText: String = ""

  // pääikkuna (anonyymi luokka johdettu MainFrame:sta)
  def top = new MainFrame():
    title = "Remote Controller"
    contents = panel
    minimumSize = new Dimension(500,400)
    centerOnScreen()
    listenTo(this, canvas.keys)
    reactions += {
      case e: WindowClosing =>
        quit()
      case KeyReleased(_,key,_,_) if key == Key.Enter =>
        serialMonitor.visible = !serialMonitor.visible
    }


  private def onTick() =
    // connection status
    Comm.currentSerial match {
      case Some(port) if port.isOpen =>
        info.foreground = new Color(0,200,0)
        info.text = port.getDescriptivePortName + " connected"
      case opt =>
        info.foreground = new Color(250,0,0)
        info.text = "No connection " + opt.map(port => s"(error code ${port.getLastErrorCode})").getOrElse("")
    }

    // vastaanota dataa Serialista
    if Comm.data_available && Comm.read_data(1000) then
      mprint("") // monitorin päivitys

      for i <- (1 to 50) do
        // etsi ensimmäinen datapaketti joka alkaa 0xA7,0x01
        Comm.dataCache.sliding(2).indexOf(Vector(0xA7, 0x01)) match {
          case index if index >= 0 =>
            Comm.dataCache.dropInPlace(index)  // poista turhat tavut alusta
            val packet = Comm.dataCache.take(32)
            if packet.length >= 32 then
              Comm.dataCache.dropInPlace(32)

              //         0xA7 = packet(0)
              //         0x01 = packet(1)
              val scan_id     = packet(2) // increments per full scan
              val fragment_id = packet(3) // increments per fragment
              //val flags       = packet(4) // bit0: is_last
              val payload_len = packet(4) // 0..25
              val crc: Int    = packet(5) << 8 | packet(6)  // (16bit) CRC16-CCITT over header-with-zeroed-crc + payload
              val scanPackets = packet.drop(7)  // 25 bytes

              mprint(s" packet (id $scan_id):\n")
              mprint(packet)
              mprint("\n")

              // lue ja parsi skannauspaketteja niin monta kuin löytyy
              while scanPackets.length >= 5 do
                val scanPacket = scanPackets.take(5); scanPackets.dropInPlace(5)
                val angledeg: Double = (scanPacket(2)<<7 | scanPacket(1)>>1) / 64.0
                val distmm: Int = (scanPacket(4)<<8 | scanPacket(3)) / 4
                mprint("  ")
                mprint( scanPacket )
                mprint(s" (${angledeg} deg ${distmm} mm)\n")
                canvas.add_measurement(angledeg, distmm / 10.0)
            end if
          case _ =>
        }

    canvas.repaint()
  end onTick


  // sulje yhteys hallitusti
  override def quit(): Unit =
    println("Quitting...")
    Comm.write_data(Comm.STOP_SCAN)
    println("Scan stopped")
    Comm.close_serial()
    super.quit()


  /** print to monitor */
  def mprint(input: Iterable[Int] | String): Unit =
    if serialMonitor.visible then
      input match {
        case bytes: Iterable[Int] =>
          bufferedSerialText += byteString(bytes)
        case str: String =>
          bufferedSerialText += str
      }
      if bufferedSerialText.length > 1000 then
        bufferedSerialText = bufferedSerialText.takeRight(1000)
      if !serialText.hasFocus then
        serialText.text = bufferedSerialText + byteString(Comm.dataCache)

end UI

