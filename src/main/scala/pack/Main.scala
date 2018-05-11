package pack

import java.io._
import java.util.jar.JarFile

import pack.jvm.common.StreamReader
import pack.jvm.struct.ClassFile

class Pack(libDir: String, clazzDir: String) {
  val libMap: Map[String, String] = scanLibJar(libDir)
  val clazzMap: Map[String, String] = scanLocalClass(clazzDir)

  libMap.foreach(println)

  // className=>jarPath
  def scanLibJar(libDir: String): Map[String, String] = {
    val dir = new File(libDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${libDir} Not A Dir")
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

  // fileName=>className
  def scanLocalClass(clazzDir: String): Map[String, String] = {
    def getFullNameFromClassFile(is: InputStream): String = {
      val sr = new StreamReader(is)
      val cf = new ClassFile(sr)
      is.close()
      cf.name + ".class"
    }

    val currentDir = new File(clazzDir).getAbsolutePath
    new File(currentDir).listFiles().filter(_.getName.endsWith(".class"))
      .map(f => (f.getName, getFullNameFromClassFile(new FileInputStream(f))))
      .toMap
  }

  def checkBackup(): Unit = {
    clazzMap.values.map(libMap(_)).toSet[String].foreach(path => {
      println(path)
      val backPath = s"${path}.bak"
      if (!new File(backPath).exists()) {
        exec(s"cp ${path} ${backPath}")
      } else {
        println(s"[${backPath}] Exists")
      }
    })
  }

  def exec(cmd: String): Unit = {
    println(cmd)
    val ex = Runtime.getRuntime.exec(cmd); //添加要进行的命令，"cmd  /c
    val br = new BufferedReader(new InputStreamReader(ex.getInputStream)) //虽然cmd命令可以直接输出，但是通过IO流技术可以保证对数据进行一个缓冲。
    Stream.continually(br.readLine()).takeWhile(_ != null).foreach(println)
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val libDir = "D:\\workspace\\scala\\scala-test\\target"
    val clazzDir = "D:\\workspace\\scala\\scala-test"
    val p = new Pack(libDir, clazzDir)
    p.checkBackup()
  }
}
