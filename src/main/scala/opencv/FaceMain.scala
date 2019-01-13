package opencv

import java.io.File

import opencv.FacePP.compare

import scala.collection.mutable

object FaceMain {

  def watch(dir: String, fn: File => Array[String]): Unit = {
    val set = mutable.Set[String]()
    while (true) {
      val nfs = new File(dir).listFiles().filter(f => !set.contains(f.getAbsolutePath))
      println(s"New Files: ${nfs.length}")
      if (set.nonEmpty) {
        set ++= nfs.flatMap(f => fn(f))
      } else {
        set ++= nfs.map(_.getAbsolutePath)
      }
      Thread.sleep(1000)
    }
  }

  def splitName(p: String): (String, String) = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(p).toArray
    val name = m.group(1)
    val ext = m.group(2)
    (name, ext)
  }

  def watchAndCompare(dir: String, target: String): Unit = watch(dir, file => {
    val ret = compare(file.getAbsolutePath, target).toString.replace(".", "_")
    val (name, ext) = splitName(file.getAbsolutePath)
    val newName = s"${name}-${ret}.${ext}"
    file.renameTo(new File(newName))
    Array(newName)
  })

  def watchAndPickFace(dir: String): Unit = watch(dir, f => pickFace(f.getAbsolutePath))

  def pickFace(path: String): Array[String] = {
    val file = new File(path)
    val landmark = FacePP.landmark(file.getAbsolutePath)
    val res = CV.drawLandmark(landmark, file.getAbsolutePath)
    Array(file.getAbsolutePath) ++ res
  }

  def main(args: Array[String]): Unit = {
    //    compare("D:/pic/liuyan/ly1.jpg", "D:/pic/liuyan/ly3.jpg")
    //    return
    val p = "E:\\Games\\PlayHome\\UserData\\Cap"
    //    val p = "D:/pic/ly2.jpg"
    new File(p).listFiles().filter(!_.isDirectory).map(_.getAbsolutePath).foreach(pickFace)
    //    val dir = "E:\\Games\\PlayHome\\UserData\\Cap"
    //    watchAndCompare(dir,"E:/ly3.jpg")
    //    watchAndPickFace(dir)
  }
}
