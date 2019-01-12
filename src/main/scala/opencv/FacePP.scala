package opencv

import java.io.File

import io.github.yuemenglong.http.HttpClient
import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.parse.JsonObj
import nu.pattern.OpenCV

import scala.collection.mutable

case class FaceRect(root: JsonObj) {
  val top: Int = root.getInt("top")
  val left: Int = root.getInt("left")
  val width: Int = root.getInt("width")
  val height: Int = root.getInt("height")
}

case class Landmark(root: JsonObj) {
  val face_rectangle: FaceRect = FaceRect(root.getObj("face").getObj("face_rectangle"))
  val landmark: JsonObj = root.getObj("face").getObj("landmark")

  val face_hairline: Array[(Int, Int)] = pickPoint(landmark.getObj("face"), "face_hairline", 144)
  val face_contour_right: Array[(Int, Int)] = pickPoint(landmark.getObj("face"), "face_contour_right", 63)
  val face_contour_left: Array[(Int, Int)] = pickPoint(landmark.getObj("face"), "face_contour_left", 63)
  val left_eyebrow: Array[(Int, Int)] = pickPoint(landmark.getObj("left_eyebrow"), "left_eyebrow", 63)
  val right_eyebrow: Array[(Int, Int)] = pickPoint(landmark.getObj("right_eyebrow"), "right_eyebrow", 63)
  val left_eye: Array[(Int, Int)] = pickPoint(landmark.getObj("left_eye"), "left_eye", 62)
  val right_eye: Array[(Int, Int)] = pickPoint(landmark.getObj("right_eye"), "right_eye", 62)
  val nose_left: Array[(Int, Int)] = pickPoint(landmark.getObj("nose"), "nose_left", 62)
  val nose_right: Array[(Int, Int)] = pickPoint(landmark.getObj("nose"), "nose_right", 62)
  val nose_midline: Array[(Int, Int)] = pickPoint(landmark.getObj("nose"), "nose_midline", 59)
  val upper_lip: Array[(Int, Int)] = pickPoint(landmark.getObj("mouth"), "upper_lip", 63)
  val lower_lip: Array[(Int, Int)] = pickPoint(landmark.getObj("mouth"), "lower_lip", 63)

  val eyes: Array[(Int, Int, Int)] = Array(eye("left"), eye("right"))

  val points: Array[(Int, Int)] =
    face_hairline ++
      face_contour_right ++
      face_contour_left ++
      left_eyebrow ++
      right_eyebrow ++
      left_eye ++
      right_eye ++
      nose_left ++
      nose_right ++
      nose_midline ++
      upper_lip ++
      lower_lip

  val minX: Int = points.map(_._1).min
  val minY: Int = points.map(_._2).min
  val maxX: Int = points.map(_._1).max
  val maxY: Int = points.map(_._2).max
  val width: Int = maxX - minX
  val height: Int = maxY - minY

  def eye(pre: String): (Int, Int, Int) = {
    val x = landmark.getObj(s"${pre}_eye").getObj(s"${pre}_eye_pupil_center").getInt("x").toInt
    val y = landmark.getObj(s"${pre}_eye").getObj(s"${pre}_eye_pupil_center").getInt("y").toInt
    val r = landmark.getObj(s"${pre}_eye").getInt(s"${pre}_eye_pupil_radius").toInt
    (x, y, r)
  }

  def pickPoint(root: JsonObj, prefix: String, max: Int): Array[(Int, Int)] = {
    (0 to max).map(i => {
      val name = s"${prefix}_${i}"
      val point = root.getObj(name)
      val x = point.getInt("x").toInt
      val y = point.getInt("y").toInt
      (x, y)
    }).toArray
  }
}

object FacePP {
  OpenCV.loadLocally()

  val client = new HttpClient
  val apiKey: (String, String) = "api_key" -> "uqngbdsbwX9CsbqPeObfwzzlaUJpPDJC"
  val apiSecret: (String, String) = "api_secret" -> "Q5kEO5lhl32wvb3mMhY0AIu90nEAob1o"

  def fileOrUrl(path: String, fileKey: String, urlKey: String): (String, Any) = {
    if (path.startsWith("http")) {
      (urlKey, path)
    } else {
      (fileKey, new File(path))
    }
  }

  def makeForm(args: (String, Any)*): Map[String, Any] = {
    (Array(apiKey, apiSecret) ++ args).toMap
  }

  def compare(p1: String, p2: String): Double = {
    val url = "https://api-cn.faceplusplus.com/facepp/v3/compare"
    val form = makeForm(
      fileOrUrl(p1, "image_file1", "image_url1"),
      fileOrUrl(p2, "image_file2", "image_url2")
    )
    val res = client.httpForm(url, form)
    println(res.getBody)
    val ret = JSON.parse(res.getBody).asObj().getDouble("confidence")
    println(ret)
    ret
  }

  def landmark(p: String): Landmark = {
    val url = "https://api-cn.faceplusplus.com/facepp/v1/face/thousandlandmark"
    val form = makeForm("return_landmark" -> "all",
      fileOrUrl(p, "image_file", "image_url")
    )
    val res = client.httpForm(url, form)
    println(res.getBody)
    Landmark(JSON.parse(res.getBody).asObj())
  }
}
