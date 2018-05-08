package test

import java.nio.ByteBuffer

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world")
    val buffer = ByteBuffer.allocate(1000)
    buffer.putInt(100)
    buffer.putDouble(10)
    println(buffer.position())
  }
}
