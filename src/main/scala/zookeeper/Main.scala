//package zookeeper
//
//import org.apache.zookeeper.{CreateMode, WatchedEvent, Watcher, ZooKeeper}
//import org.apache.zookeeper.data.ACL
//import java.util
//
//import org.apache.zookeeper.ZooDefs.{Ids, Perms}
//import org.apache.zookeeper.data.ACL
//import org.apache.zookeeper.data.Id
//import org.apache.zookeeper.server.auth.DigestAuthenticationProvider
//
////noinspection ConvertExpressionToSAM
//object Main {
//  def main(args: Array[String]): Unit = {
//    val url = "211.159.173.48:2181"
//    val zk = new ZooKeeper(url, 1000000, new Watcher {
//      override def process(event: WatchedEvent): Unit = {
//        println(s"Watcher Process, ${event}")
//      }
//    })
//
//    val path = "/test"
//    zk.exists(path, new Watcher {
//      override def process(event: WatchedEvent): Unit = {
//        println(s"Watcher Process, ${event}")
//      }
//    }) match {
//      case null => zk.create(path, "test".getBytes, Ids.OPEN_ACL_UNSAFE, CreateMode.EPHEMERAL)
//      case stat => zk.delete(path, stat.getVersion)
//    }
//    Thread.sleep(4000)
//  }
//}
