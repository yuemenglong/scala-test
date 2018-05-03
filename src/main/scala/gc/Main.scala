//package gc
//
//import java.nio.ByteBuffer
//import java.util
//
//object Main {
//
//  def main(args: Array[String]): Unit = {
//    //    var t = new Test
//    //    t = null
//    ////    System.gc()
//    //    Thread.sleep(1000)
//    val list = new util.ArrayList[Object]()
//    while (true) {
//      val b = ByteBuffer.allocate(1024)
//      list.add(b)
//      Thread.sleep(1)
//    }
//  }
//}
//
//class Test {
//  override def finalize(): Unit = {
//    println("finalize")
//  }
//}
