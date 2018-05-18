package analyze

import java.io.{File, FileOutputStream}

import scala.io.Source

object Main {
  def readSource(path: String): Iterator[String] = {
    val stream = Thread.currentThread().getContextClassLoader.getResourceAsStream(path)
    Source.fromInputStream(stream).getLines()
  }

  def sendAndRecv(t: String): Unit = {
    //    2018-05-16 11:26:00,402 DEBUG DeamonSender YML Send count:295, bytes:62675
    val os = new FileOutputStream(s"D:/${t}.csv")
    readSource(s"xdr3/${t}.txt").map(line => {
      val time = line.split(",")(0)
      val count = line.split(" ")(6).split(":")(1)
      val bytes = line.split(" ")(7).split(":")(1)
      Array(time, count, bytes).mkString(",")
    }).foreach(line => os.write(s"${line}\n".getBytes()))
  }

  def gc(): Unit = {
    //    2018-05-16 11:27:47
    //    S0C    S1C    S0U    S1U      EC       EU        OC         OU       MC     MU    CCSC   CCSU   YGC     YGCT    FGC    FGCT     GCT
    //    2096640.0 2096640.0 14896.6  0.0   12583424.0 7319983.4   1536.0      95.5    21248.0 20445.0  0.0    0.0        2    0.016   1      0.081    0.098
    val os = new FileOutputStream("D:/gc.csv")
    readSource("xdr3/gc.txt").zipWithIndex.map { case (line, no) =>
      (no / 3, line)
    }.toArray.groupBy(_._1).toArray.sortBy(_._1).map(_._2.map(_._2)).map(arr => {
      val Array(t, _, d) = arr
      val items = d.split("""\s+""")
      s"${t},${items(4)},${items(5)},${items(6)},${items(7)},"
    }).foreach(line => os.write(s"${line}\n".getBytes()))
  }

  def gcutil(): Unit = {
    //    2018-05-16 16:58:12
    //    S0     S1     E      O      M     CCS    YGC     YGCT    FGC    FGCT     GCT
    //    0.00   0.00  14.60  26.91  94.80      -     83   46.644   193 4424.167 4470.811
    val os = new FileOutputStream("D:/gcutil.csv")
    readSource("xdr3/gcutil.txt").zipWithIndex.map { case (line, no) =>
      (no / 3, line)
    }.toArray.groupBy(_._1).toArray.sortBy(_._1).map(_._2.map(_._2)).map(arr => {
      val Array(t, _, d) = arr
      val items = d.split("""\s+""")
      s"${t},${items(7)},${items(9)},"
    }).foreach(line => os.write(s"${line}\n".getBytes()))
  }

  def cpu(): Unit = {
    val os = new FileOutputStream("D:/cpu.csv")
    readSource("xdr3/cpu.txt").zipWithIndex.map { case (line, no) =>
      (no / 10, line)
    }.toArray.groupBy(_._1).toArray.sortBy(_._1).map(_._2.map(_._2)).map(arr => {
      val t = arr(0)
      val data = arr(8)
      val items = data.split("""\s+""").reverse
      s"${t},${items(3)}"
      //      s"${t},${items(7)},${items(9)},"
    }).foreach(line => os.write(s"${line}\n".getBytes()))

  }

  def dump(): Unit = {
    val dir = Thread.currentThread().getContextClassLoader.getResource("xdr3/dump").getFile
    val os = new FileOutputStream("D:/dump.csv")
    new File(dir).listFiles().map(f => {
      val items = f.getName.replace(".txt", "").split("[-_: ]")
      val time = s"${items(0)}-${items(1)}-${items(2)} ${items(3)}:${items(4)}:${items(5)}"
      val data = Source.fromFile(f).getLines().filter(line => line.contains("StatePusher") && !line.contains("$")).toArray
      val Array(_, _, count, bytes, _) = data(0).split("""\s+""")
      s"${time},${count},${bytes},${bytes.toDouble / count.toDouble}"
    }).foreach(line => {
      os.write(s"${line}\n".getBytes())
    })
  }

  def main(args: Array[String]): Unit = {
//    sendAndRecv("send")
//    sendAndRecv("recv")
//    gc()
//    gcutil()
    cpu()
//    dump()
  }
}
