import com.fazecast.jSerialComm.*
import Console.{GREEN, RED, RESET}
import scala.collection.mutable.Buffer
import scala.io.StdIn.readLine

object Comm:
  /** Tällä hetkellä auki oleva portti tai None */
  var currentSerial: Option[SerialPort] = None

  def open_serial() =
    println("Available ports:")
    val availPorts = SerialPort.getCommPorts
    for (port,i) <- availPorts.zipWithIndex do
      println(s"[$i] " + port.getPortDescription)

    // pyydä käyttäjää valitsemaan portti
    var selectedPort = -1
    if availPorts.nonEmpty then
      selectedPort = readLine("Select port: ").toInt

    availPorts.lift(selectedPort).foreach(port =>
      // avataan valittu portti
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
      println("port <" + port.getSystemPortName + "> closed")
    )


  def data_available: Boolean =
    this.currentSerial.exists(port => port.bytesAvailable() > 0)


  def read_data(numBytes: Int): Vector[Byte] =
    val out_data = Buffer[Byte]()

    this.currentSerial.foreach(port =>
      val dataBuf = Array.ofDim[Byte](numBytes)
      port.readBytes(dataBuf, numBytes) match {
        case numBytesRead if numBytesRead > 0 =>
          println(s"<- $numBytesRead bytes read")
          for i <- (0 to numBytesRead) do out_data += dataBuf(i)
        case 0 =>
          println("no data to read")
        case -1 =>
          println(s"${RED}failed to read data (error code ${port.getLastErrorCode})${RESET}")
      })

    out_data.toVector


  def write_data(data: Vector[Byte]): Boolean =
    this.currentSerial.exists(port =>
      port.writeBytes(data.toArray, data.length) match {
        case numBytesWritten if numBytesWritten >= 0 =>
          println(s"-> $numBytesWritten bytes written")
          true
        case -1 =>
          println(s"${RED}failed to write data (error code ${port.getLastErrorCode})${RESET}")
          false
      })

end Comm
