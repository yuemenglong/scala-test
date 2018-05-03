//package rocksdb
//
//import java.io.File
//
//import org.rocksdb.{Options, RocksDB}
//
//import scala.util.Random
//
//object Main {
//  def main(args: Array[String]): Unit = {
//        System.loadLibrary("librocksdbjni-win64")
//    //    val p = new File("t").getAbsolutePath
//    //    println(p)
//    //    val path = System.getProperty("java.library.path")
//    //    println(path)
//    try {
//      RocksDB.loadLibrary()
//      val opt = new Options().setCreateIfMissing(true)
//      val db = RocksDB.open(opt, "D:/rocksdb")
//      (1 to 10000).foreach(i => {
//        db.put(Random.nextDouble().toString.getBytes(), i.toString.getBytes())
//      })
//    } catch {
//      case ex: Throwable =>
//        println(ex.getMessage)
//    }
//  }
//}
