package pcs

import java.io._

import scala.collection.mutable.ArrayBuffer

object PCS {
  val pcs = "D:/pcs/BaiduPCS-Go"

  def exec(cmd: String): Array[String] = {
    val s = s"${pcs} ${cmd}"
    println(s)
    val proc = Runtime.getRuntime.exec(s)
    val br = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val ab = new ArrayBuffer[String]()
    Stream.continually({
      br.readLine()
    }).takeWhile(_ != null).foreach(s => {
      println(s)
      ab += s
    })
    proc.waitFor()
    ab.toArray
  }

  def ls(dir: String): Array[String] = {
    exec(s"ls ${dir} -time -desc").drop(4).filter(s => {
      s.trim.split("""\s+""")(0).matches("""\d+""")
    }).map(s => {
      val name = s.trim.split("""\s+""").last
      s"${dir}/${name}"
    })
  }

  def download(path: String): Array[String] = exec(s"download ${path}")
}

object Main {
  def main(args: Array[String]): Unit = {
    //    val dir = "/pcs"
    //    val newest = PCS.exec(s"ls ${dir} -time -desc").slice(4, 5)
    //    println(newest)
    //    val name = newest(0).split("""\s+""").last
    //    val path = s"${dir}/${name}"
    //    PCS.exec(s"download ${path}")
    //    PCS.ls("/pcs").foreach(println)
    PCS.download("/pcs/playhome新版正式中文整合/")
  }
}
