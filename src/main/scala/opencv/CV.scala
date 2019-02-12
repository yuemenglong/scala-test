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

  def splitPath(in: String): (String, String) = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(in).toArray
    val name = m.group(1)
    val ext = m.group(2)
    (name, ext)
  }

  def outPath(in: String): String = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(in).toArray
    val name = m.group(1)
    val ext = m.group(2)
    val out = s"${name}-out.${ext}"
    out
  }

  def outFacePath(in: String): String = {
    val (name, ext) = splitPath(in)
    s"${name}-out-face.${ext}"
  }

  def outEyePath(in: String): String = {
    val (name, ext) = splitPath(in)
    s"${name}-out-eye.${ext}"
  }

  def outMousePath(in: String): String = {
    val (name, ext) = splitPath(in)
    s"${name}-out-mouth.${ext}"
  }

  def zoomWidth(in: Size, width: Int): Size = {
    val height = (in.height * 1.0 / in.width * width).toInt
    new Size(width, height)
  }

  def zoomHeight(in: Size, height: Int): Size = {
    val width = (in.width * 1.0 / in.height * height).toInt
    new Size(width, height)
  }

  def drawLandmark(landmark: Landmark, in: String, color: Scalar = RED): Array[String] = {
    val out = outPath(in)
    val outFace = outFacePath(in)
    val outEye = outEyePath(in)
    val outMouse = outMousePath(in)

    {
      val mat = Imgcodecs.imread(in)

      {
        val eyeMat = new Mat(mat, landmark.eye_rect)
        val newEyeSize = zoomWidth(eyeMat.size(), 1080)
        val newEyeMat = new Mat
        Imgproc.resize(eyeMat, newEyeMat, newEyeSize)
        Imgcodecs.imwrite(outEye, newEyeMat)
      }

      {
        val mouseMat = new Mat(mat, landmark.mouth_rect)
        val newMouseSize = zoomWidth(mouseMat.size(), 1080)
        val newMouseMat = new Mat
        Imgproc.resize(mouseMat, newMouseMat, newMouseSize)
        Imgcodecs.imwrite(outMouse, newMouseMat)
      }

      landmark.points.foreach { case (x, y) =>
        Imgproc.circle(mat, new Point(x, y), 1, color)
      }
      landmark.eyes.foreach { case (x, y, r) =>
        Imgproc.circle(mat, new Point(x, y), r, color)
      }
      Imgcodecs.imwrite(out, mat)
    }

    {
      val size = new Size(landmark.face_rect.width, landmark.face_rect.height)
      val left = landmark.face_rect.x
      val top = landmark.face_rect.y
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

    Array(out, outFace, outEye, outMouse)
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
