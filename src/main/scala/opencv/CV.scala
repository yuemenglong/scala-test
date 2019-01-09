package opencv

import java.io.File
import java.nio.file.Paths

import nu.pattern.OpenCV
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import io.github.yuemenglong.http.HttpClient
import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.parse.JsonObj

object CV {
  OpenCV.loadLocally()
  val BLUE: Scalar = new Scalar(255, 0, 0)
  val GREEN: Scalar = new Scalar(0, 255, 0)
  val RED: Scalar = new Scalar(0, 0, 255)
  val BLACK: Scalar = new Scalar(0, 0, 0)
  val WHITE: Scalar = new Scalar(255, 255, 255)

  def main(args: Array[String]): Unit = {
    //    compare("D:/fbb1.jpg", "D:/fbb2.jpg")
    //    keyPoint("D:/gxt3.jpg", "D:/gxt31.jpg")
    val p = "D:/pic/cym1.jpg"
    val lm = FacePP.landmark(p)
    drawLandmark(lm, p)
  }

  def drawLandmark(landmark: Landmark, in: String): Unit = {
    val Array(m) = """(.*)\.([^.]+)""".r.findAllMatchIn(in).toArray
    val name = m.group(1)
    val ext = m.group(2)
    val out = s"${name}-out.${ext}"
    val outBlank = s"${name}-out-blank.${ext}"

    val mat = Imgcodecs.imread(in)
    landmark.points.foreach { case (x, y) =>
      Imgproc.circle(mat, new Point(x, y), 1, RED)
    }
    landmark.eyes.foreach { case (x, y, r) =>
      Imgproc.circle(mat, new Point(x, y), r, RED)
    }
    Imgcodecs.imwrite(out, mat)

    mat.setTo(WHITE)
    landmark.points.foreach { case (x, y) =>
      Imgproc.circle(mat, new Point(x, y), 1, RED)
    }
    landmark.eyes.foreach { case (x, y, r) =>
      Imgproc.circle(mat, new Point(x, y), r, RED)
    }
    Imgcodecs.imwrite(outBlank, mat)
  }

  def cv(): Unit = {
    OpenCV.loadLocally()
    val mat = Imgcodecs.imread("D:/a.png")
    Imgproc.line(mat, new Point(0, 0), new Point(200, 300), new Scalar(255, 0, 0))
    Imgcodecs.imwrite("D:/b.png", mat)
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
