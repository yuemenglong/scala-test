package opencv

import java.io.File

import opencv.FacePP.compare

import scala.collection.mutable

object FaceMain {

  def watch(dir: String, fn: File => Array[String]): Unit = {
    var first = true
    val set = mutable.Set[String]()
    while (true) {
      val nfs = new File(dir).listFiles().filter(f => !set.contains(f.getAbsolutePath))
      println(s"New Files: ${nfs.length}")
      if (nfs.nonEmpty && !first) {
        set ++= nfs.flatMap(f => fn(f))
      } else {
        set ++= nfs.map(_.getAbsolutePath)
      }
      first = false
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
    try {
      val file = new File(path)
      val landmark = FacePP.landmark(file.getAbsolutePath)
      val res = CV.drawLandmark(landmark, file.getAbsolutePath)
      Array(file.getAbsolutePath) ++ res
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Array(path)
    }
  }

  def main(args: Array[String]): Unit = {
    //    val p = "D:/temp/wj.jpg"
    //    pickFace(p)
    val p = "E:\\Games\\PlayHome\\UserData\\Cap"
    watchAndPickFace(p)
    //    pickFace(s"${p}/1.jpg")
  }
}
