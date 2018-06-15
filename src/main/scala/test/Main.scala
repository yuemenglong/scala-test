package test

import java.{lang, util}
import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer
import java.net.URI
import java.nio.file._
import java.io._
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.ZipOutputStream

object PCS {
  val exe = "D:/pcs/BaiduPCS-Go"

  def exec(cmd: String): Array[String] = {
    val s = s"${exe} ${cmd}"
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
}

object Main {
  def main(args: Array[String]): Unit = {
    val dir = "/pcs"
    val newest = PCS.exec(s"ls ${dir} -time -desc").slice(4, 5)
    println(newest)
    val name = newest(0).split("""\s+""").last
    val path = s"${dir}/${name}"
    PCS.exec(s"download ${path}")
  }
}
