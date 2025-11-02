import com.fazecast.jSerialComm.*
import Console.{GREEN, RED, RESET}
import scala.collection.mutable.Buffer

object Comm:
  /** Tällä hetkellä auki oleva portti tai None */
  var currentSerial: Option[SerialPort] = None

  def open_serial() =
    println("Available ports:")
    for port <- SerialPort.getCommPorts do
      println(port.getPortDescription)

    SerialPort.getCommPorts.headOption.foreach(port =>
      // avataan (ensimmäinen läydetty) portti
      port.openPort(1000)

      if port.isOpen then
        println(s"${GREEN}" + "port <" + port.getSystemPortName + "> opened" + s"${RESET}")
        // tärkeät asetukset
        port.setBaudRate(115200)
        port.setComPortTimeouts(SerialPort.TIMEOUT_NONBLOCKING, 3000, 1000)
        this.currentSerial = Some(port)
      else
        println(s"${RED}" + "Failed to open port\nError code: " + port.getLastErrorCode.toString + s"${RESET}")
    )

  def close_serial() =
    this.currentSerial.foreach(port =>
      port.closePort()
      println("port closed")
    )

  def data_available: Boolean =
    this.currentSerial.exists(port => port.bytesAvailable() > 0)

  def read_data(numBytes: Int): Vector[Byte] =
    val dataBuf = Array.ofDim[Byte](numBytes)
    val out_data = Buffer[Byte](0x42)
    this.currentSerial.foreach(port =>
      port.readBytes(dataBuf, numBytes) match {
        case numBytesRead if numBytesRead > 0 =>
          println("data read: " + dataBuf.map(_.toChar).mkString)
          for i <- (0 to numBytesRead) do out_data += dataBuf(i)
        case 0 =>
          println("no data to read")
        case -1 =>
          println(s"${RED}error reading data${RESET}")
      })
    out_data.toVector

end Comm
