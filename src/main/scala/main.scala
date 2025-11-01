import com.fazecast.jSerialComm.*
import Console.{GREEN, RED, RESET}


def open_serial(): Option[SerialPort] =
  var opened_port: Option[SerialPort] = None
  for port <- SerialPort.getCommPorts do
    println("portti " + port.toString)

    port.openPort(1000)

    if port.isOpen then
      println(s"${RESET}${GREEN}" + "port opened" + s"${RESET}")
      port.setBaudRate(115200)
      port.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 3000, 1000)
      opened_port = Some(port)
    else
      println(s"${RESET}${RED}" + "Failed to open port\nError code: " + port.getLastErrorCode.toString + s"${RESET}")
  opened_port

def try_read_byte(port: SerialPort): Option[Byte] =
  if port.bytesAvailable() > 0 then
    val data = Array.ofDim[Byte](1)
    port.readBytes(data, data.length) match {
      case numBytes if numBytes > 0 =>
        println("data read: " + data.map(_.toChar).mkString)
        return data.lift(0)
      case 0 =>
        println("no data to read")
        return None
      case -1 =>
        println(s"${RED}error reading data")
        return None
    }
  else
    None


@main def main(): Unit =
  open_serial() match {
    case Some(port) =>
      val loopStartTime = System.currentTimeMillis()

      while System.currentTimeMillis() < loopStartTime + 5000 do
        try_read_byte(port)
      end while

      port.closePort()
    case None =>
      ()
  }
  println("loppu")
