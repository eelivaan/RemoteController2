import com.fazecast.jSerialComm.*
import Console.{GREEN, RED, RESET}
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

// reinterpret cast
def byteToInt(x: Byte): Int = if x < 0 then 256 + x.toInt else x.toInt
def intToByte(x: Int): Byte = x.toByte

def byteString(x: Iterable[Int]) = x.map(v => f"0x${v}%02X").mkString(" ")

val demo = ListBuffer[Int]() /*
  ++ ListBuffer[Int](1,2,3,4,
  0xA7,
  0x01,
  0x0,
  0x0,
  // 0x0,
  24,
  0xFF, 0xFF)
  ++ (for i <- (0 to 20 by 5) yield Vector(0,i*64<<1|1,0,250*40,0)).flatten
*/


object Comm:
  /** Tällä hetkellä auki oleva portti tai None */
  var currentSerial: Option[SerialPort] = None

  /** Bufferoitu data, päivitetään aina read_data():ssa */
  val dataCache = ListBuffer[Int]()

  // huom:
  // java Byte = 8 bit signed int [-128 127]
  // Arduino Byte = 8 bit unsigned int [0 255]

  // komennot Lidarille
  val START_SCAN = Vector[Int](0xA5, 0x20)
  val START_SCAN_RESPONSE = Vector[Int](0xA5, 0x5A, 0x05, 0x00, 0x00, 0x40, 0x81)
  val STOP_SCAN = Vector[Int](0xA5, 0x25)


  def open_serial() =
    println("Available ports:")
    val availPorts = SerialPort.getCommPorts
    for (port,i) <- availPorts.zipWithIndex do
      println(s"[$i] " + port.getDescriptivePortName)

    // pyydä käyttäjää valitsemaan portti
    var selectedPort = 0
    if availPorts.length > 1 then
      selectedPort = readLine("Select port: ").toInt

    availPorts.lift(selectedPort).foreach(port =>
      // avataan valittu portti
      port.openPort(1000)

      if port.isOpen then
        println(s"${GREEN}" + "port <" + port.getSystemPortName + "> opened" + s"${RESET}")
        // tärkeät asetukset
        //port.setBaudRate(115200)
        port.setBaudRate(1000000)
        port.setComPortTimeouts(SerialPort.TIMEOUT_NONBLOCKING, 3000, 1000)
        this.currentSerial = Some(port)
      else
        println(s"${RED}" + "Failed to open port\nError code: " + port.getLastErrorCode.toString + s"${RESET}")

      demo.clear()
    )


  def close_serial() =
    this.currentSerial.foreach(port =>
      port.closePort()
      println("port <" + port.getSystemPortName + "> closed")
    )


  def data_available: Boolean =
    if demo.nonEmpty then
      return true
    this.currentSerial.exists(port => port.bytesAvailable() > 0)


  def read_data(numBytes: Int): Boolean =
    if demo.nonEmpty then
      this.dataCache ++= demo.take(1); demo.dropInPlace(1)
      return true

    this.currentSerial.exists(port =>
      val dataBuf = Array.ofDim[Byte](numBytes)
      port.readBytes(dataBuf, numBytes) match {
        case numBytesRead if numBytesRead > 0 =>
          //println(s"<- $numBytesRead bytes read")
          for i <- (0 until numBytesRead) do
            this.dataCache += byteToInt(dataBuf(i))
          true
        case 0 =>
          println("no data to read")
          false
        case -1 =>
          println(s"${RED}failed to read data (error code ${port.getLastErrorCode})${RESET}")
          false
      })


  def write_data(data: Vector[Int]): Boolean =
    UI.mprint("-> send " + byteString(data) + "\n")

    this.currentSerial.exists(port =>
      val byteArray = data.map[Byte](x => intToByte(x)).toArray
      port.writeBytes(byteArray, byteArray.length) match {
        case numBytesWritten if numBytesWritten >= 0 =>
          //println(s"-> $numBytesWritten bytes written")
          true
        case -1 =>
          println(s"${RED}failed to write data (error code ${port.getLastErrorCode})${RESET}")
          false
      })

end Comm
