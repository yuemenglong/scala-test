package test

import java.lang
import java.nio.ByteBuffer

import com.carrotsearch.sizeof.RamUsageEstimator

import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val arr = new ArrayBuffer[Object]()
    (1 to 1000000).foreach(i => {
      val s = new s1mme_bearer
      s.init()
      arr += s
    })
    Thread.sleep(1000000000)
    println(arr.length)
  }
}
