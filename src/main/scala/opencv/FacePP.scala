package opencv

import java.io._

import io.github.yuemenglong.http.HttpClient
import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.parse.JsonObj
import nu.pattern.OpenCV
import org.opencv.core.Rect
import sun.misc.BASE64Decoder

case class FaceRect(top: Int, left: Int, width: Int, height: Int) {
  def toReqString = s"${top},${left},${width},${height}"
}

object FaceRect {
  def apply(root: JsonObj): FaceRect = FaceRect(
    root.getInt("top"),
    root.getInt("left"),
    root.getInt("width"),
    root.getInt("height"))
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

  val face_rect: Rect = {
    val minX: Int = points.map(_._1).min
    val minY: Int = points.map(_._2).min
    val maxX: Int = points.map(_._1).max
    val maxY: Int = points.map(_._2).max
    new Rect(minX, minY, maxX - minX, maxY - minY)
  }

  val eye_rect: Rect = {
    val minYt: Int = (left_eye ++ right_eye).map(_._2).min
    val maxYt: Int = (left_eye ++ right_eye).map(_._2).max
    val minY = minYt - (maxYt - minYt) / 2
    val maxY = maxYt + (maxYt - minYt) / 2
    val facePs = (face_hairline ++ face_contour_left ++ face_contour_right).filter { case (_, y) =>
      minY <= y && y <= maxY
    }
    val minX = facePs.map(_._1).min
    val maxX = facePs.map(_._1).max
    new Rect(minX, minY, maxX - minX, maxY - minY)
  }

  val mouth_rect: Rect = {
    val minYt: Int = (upper_lip ++ lower_lip).map(_._2).min
    val maxYt: Int = (upper_lip ++ lower_lip).map(_._2).max
    val minY = minYt - (maxYt - minYt) / 2
    val maxY = maxYt + (maxYt - minYt) / 2
    val facePs = (face_hairline ++ face_contour_left ++ face_contour_right).filter { case (_, y) =>
      minY <= y && y <= maxY
    }
    val minX = facePs.map(_._1).min
    val maxX = facePs.map(_._1).max
    new Rect(minX, minY, maxX - minX, maxY - minY)
  }

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

  def detect(p: String): Array[FaceRect] = {
    val url = "https://api-cn.faceplusplus.com/facepp/v3/detect"
    val form = makeForm(
      fileOrUrl(p, "image_file", "image_url")
    )
    val res = client.httpForm(url, form)
    println(res.getBody)
    val root = JSON.parse(res.getBody).asObj()
    root.getArr("faces").array.map(jo => {
      val rect = jo.asObj().getObj("face_rectangle")
      FaceRect(rect)
    })
  }

  def mergeface(p1: String, rect1: FaceRect, p2: String, rect2: FaceRect, mergeRate: Int = 50): String = {
    val url = "https://api-cn.faceplusplus.com/imagepp/v1/mergeface"
    val form = makeForm(
      fileOrUrl(p1, "template_file", "template_url"),
      fileOrUrl(p2, "merge_file", "merge_url"),
      ("template_rectangle", rect1.toReqString),
      ("merge_rectangle", rect2.toReqString)
    )
    val res = client.httpForm(url, form)
    println(res.getBody)
    val result = res.getBody.split(""""result":""")(1).trim.split(""""""")
    result(1)
    //    val re = """.*"result": "([^"]+)".*""".r
    //    res.getBody match {
    //      case re(_, result) => result
    //      case _ => ""
    //    }
    //    JSON.parse(res.getBody).asObj().getStr("result")
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

  def base64ToImage(imgStr: String, imgFilePath: String) { // 对字节数组字符串进行Base64解码并生成图片
    val decoder = new BASE64Decoder()
    val b: Array[Byte] = decoder.decodeBuffer(imgStr).map {
      case c if c < 0 => (c + 256).toByte
      case c => c
    }
    val out = new FileOutputStream(imgFilePath)
    out.write(b)
    out.flush()
    out.close()
  }


  def main(args: Array[String]): Unit = {
    //    FaceMain.watchAndCompare("E:\\Games\\PlayHome\\UserData\\Cap", "E:/ly3.jpg")
    //    compare("E:/1.jpg", "E:/ly3.jpg")

    val p = "https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1553252618968&di=b65c6c70de911b238450a2515a13ed35&imgtype=0&src=http%3A%2F%2Fimage2.sina.com.cn%2Fent%2Fd%2F2005-01-18%2FU92P28T3D633859F326DT20050118143536.jpg"
    val res = detect(p)
    val b64 = mergeface(p, res(0), p, res(1))
    base64ToImage(b64, "D:/out2.jpg")

    //    val s = new BufferedReader(new InputStreamReader(new FileInputStream("D:/out.txt")))
    //    val json = s.readLine()
    //    val result = JSON.parse(json).asObj().getStr("result")
    //    println(result)
  }

  /*
  * curl -X POST "https://api-cn.faceplusplus.com/imagepp/v1/mergeface" \
-F "api_key=uqngbdsbwX9CsbqPeObfwzzlaUJpPDJC"  \
-F "api_secret=Q5kEO5lhl32wvb3mMhY0AIu90nEAob1o"  \
-F "template_url=https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1553252618968&di=b65c6c70de911b238450a2515a13ed35&imgtype=0&src=http%3A%2F%2Fimage2.sina.com.cn%2Fent%2Fd%2F2005-01-18%2FU92P28T3D633859F326DT20050118143536.jpg" \
-F "template_rectangle=126,286,62,62" \
-F "merge_url=https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1553252618968&di=b65c6c70de911b238450a2515a13ed35&imgtype=0&src=http%3A%2F%2Fimage2.sina.com.cn%2Fent%2Fd%2F2005-01-18%2FU92P28T3D633859F326DT20050118143536.jpg" \
-F "merge_rectangle=171,200,57,57" \
-F "merge_rate=70"
  * */
}
