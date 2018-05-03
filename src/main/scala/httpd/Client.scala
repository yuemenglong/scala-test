package httpd

import java.io.FileOutputStream
import java.net.Socket

object Client {
  def main(args: Array[String]): Unit = {
    val host = "10.56.71.167"
    val remote = "/root/log11/send.tar.gz"
    val local = "D:/send.tar.gz"
    val socket = new Socket(host, 50200)
    socket.getOutputStream.write(s"${remote}\n".getBytes())
    val os = new FileOutputStream(local)
    Server.pipe(socket.getInputStream, os)
    socket.close()
    os.close()
  }
}
