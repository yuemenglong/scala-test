package opencv

import nu.pattern.OpenCV
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

object CV {
  OpenCV.loadLocally()
  val BLUE: Scalar = new Scalar(255, 0, 0)
  val GREEN: Scalar = new Scalar(0, 255, 0)
  val RED: Scalar = new Scalar(0, 0, 255)
  val BLACK: Scalar = new Scalar(0, 0, 0)
  val WHITE: Scalar = new Scalar(255, 255, 255)

  def outPath(in: String): String = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(in).toArray
    val name = m.group(1)
    val ext = m.group(2)
    val out = s"${name}-out.${ext}"
    out
  }

  def outFacePath(in: String): String = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(in).toArray
    val name = m.group(1)
    val ext = m.group(2)
    val outFace = s"${name}-out-face.${ext}"
    outFace
  }

  def drawLandmark(landmark: Landmark, in: String, color: Scalar = RED): Array[String] = {
    val out = outPath(in)
    val outFace = outFacePath(in)

    {
      val mat = Imgcodecs.imread(in)
      landmark.points.foreach { case (x, y) =>
        Imgproc.circle(mat, new Point(x, y), 1, color)
      }
      landmark.eyes.foreach { case (x, y, r) =>
        Imgproc.circle(mat, new Point(x, y), r, color)
      }
      Imgcodecs.imwrite(out, mat)
    }

    {
      val size = new Size(landmark.width, landmark.height)
      val left = landmark.minX
      val top = landmark.minY
      val mat = new Mat(size, CvType.CV_64FC3, WHITE)
      landmark.points.foreach { case (x, y) =>
        val xx = x - left
        val yy = y - top
        Imgproc.circle(mat, new Point(xx, yy), 1, color)
      }
      landmark.eyes.foreach { case (x, y, r) =>
        val xx = x - left
        val yy = y - top
        Imgproc.circle(mat, new Point(xx, yy), r, color)
      }
      val resized = new Mat
      val resizeHeight = 1440
      val resizeWidth = (size.width * 1.0 / size.height * resizeHeight).toInt
      Imgproc.resize(mat, resized, new Size(resizeWidth, resizeHeight))
      Imgcodecs.imwrite(outFace, resized)
    }

    Array(out, outFace)
  }

  def drawPoint(in: String, out: String, points: (Int, Int)*): Unit = {
    val mat = Imgcodecs.imread(in)
    points.foreach { case (x, y) =>
      Imgproc.circle(mat, new Point(x, y), 1, new Scalar(0, 0, 255))
    }
    Imgcodecs.imwrite(out, mat)
  }

  def drawEmptyLine(in: String, out: String, points: (Int, Int)*): Unit = {
    OpenCV.loadLocally()
    val mat = Imgcodecs.imread(in)
    mat.setTo(new Scalar(0, 0, 0))
    points.dropRight(1).zip(points.drop(1)).foreach { case ((x1, y1), (x2, y2)) =>
      Imgproc.line(mat, new Point(x1, y1), new Point(x2, y2), new Scalar(0, 0, 255), 2, Imgproc.LINE_AA, 0)
    }
    //    points.foreach { case (x, y) =>
    //      Imgproc.circle(mat, new Point(x, y), 1, new Scalar(0, 0, 255))
    //    }
    Imgcodecs.imwrite(out, mat)
  }

}
