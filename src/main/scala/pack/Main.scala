package pack

import java.io.{File, InputStream}
import java.util.jar.JarFile

object Main {
  def scanJar(jarDir: String): Map[String, String] = {
    val dir = new File(jarDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${jarDir} Not A Dir")
    }
    dir.listFiles().filter(_.getName.endsWith(".jar")).flatMap(file => {
      val jarFile = new JarFile(file)
      val entries = jarFile.entries()
      Stream.continually({
        entries.hasMoreElements match {
          case true => entries.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).filter(_.getName.endsWith(".class"))
        .map(f => (f.getName, file.getAbsolutePath))
    }).toMap
  }

  def scanLocalClass(): Map[String, String] = {
    val currentDir = new File("").getAbsolutePath
    new File(currentDir).listFiles().filter(_.getName.endsWith(".class"))
      .foreach(f => println(f.getName))
    null
  }

  def getFullNameFromClassFile(is: InputStream): String = {
    null
  }

  def main(args: Array[String]): Unit = {
    //    scanJar("D:\\workspace\\scala\\scala-test\\target")
    //      .foreach(println)
    scanLocalClass()
  }
}
