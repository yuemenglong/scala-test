package sleep

object Main {
  def main(args: Array[String]): Unit = {
    (1 to 60).foreach(i => {
      println(s"Count ${i}")
      Thread.sleep(1000)
    })
  }
}
