package httpd

import java.io._
import java.net.ServerSocket

object Server {
  def pipe(is: InputStream, os: OutputStream): Unit = {
    val buffer = new Array[Byte](4096)
    var len = is.read(buffer, 0, buffer.length)
    var total = 0
    while (len >= 0) {
      os.write(buffer, 0, len)
      total += len
      println(s"Pipe ${total}")
      len = is.read(buffer, 0, buffer.length)
    }
  }

  def main(args: Array[String]): Unit = {
    val port = try {
      args(args.length - 1).toInt
    } catch {
      case _: Throwable => 50200
    }
    val server = new ServerSocket(port)
    println(s"Listen On ${port}")
    while (true) {
      println("==========================")
      val socket = server.accept()
      println("Connect Create")
      try {
        val br = new BufferedReader(new InputStreamReader(socket.getInputStream))
        val path = br.readLine()
        println(path)
        if (new File(path).isFile) {
          pipe(new FileInputStream(path), socket.getOutputStream)
        } else {
          println("No Such File")
        }
      } catch {
        case _: Throwable =>
      } finally {
        println("Connect Close")
        socket.close()
      }
    }
  }
}
