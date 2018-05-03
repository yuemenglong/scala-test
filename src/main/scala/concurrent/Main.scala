//package concurrent
//
//import java.util
//import java.util.concurrent._
//
//object Main {
//  def main(args: Array[String]): Unit = {
//    val start = System.currentTimeMillis()
//    val executor = Executors.newFixedThreadPool(8)
//    (1 to 8).foreach(i => {
//      executor.submit(new Worker(i))
//    })
//    executor.shutdown()
//    val s = executor.awaitTermination(Long.MaxValue, TimeUnit.SECONDS)
//    val end = System.currentTimeMillis()
//    println(s"Finish All Jobs, ${s}, Time Use ${end - start}")
//
//  }
//}
//
//object Worker {
//  val list: ConcurrentLinkedQueue[Int] = new ConcurrentLinkedQueue[Int]()
//  val list2: LinkedBlockingQueue[Int] = new LinkedBlockingQueue[Int]()
//}
//
//class Worker(val id: Int) extends Thread {
//  //    val list: util.Queue[Int] = new ConcurrentLinkedQueue[Int]()
//  //  val list: util.Queue[Int] = Worker.list
//  val list: util.Queue[Int] = Worker.list2
//
//  override def run(): Unit = {
//    println(s"[${id}] Start")
//    (1 to 1000000).foreach(i => {
//      if (Math.random() < 0.5) {
//        list.poll()
//      } else {
//        list.offer(i)
//      }
//    })
//    println(s"[${id}] Finish, List Size ${list.size()}")
//  }
//}