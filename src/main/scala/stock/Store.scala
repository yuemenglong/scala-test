package stock

import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date

import io.github.yuemenglong.http.HttpClient
import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.lang.JsonDate

/**
  * Created by <yuemenglong@126.com> on 2018/8/5.
  */
class Bar {
  @JsonDate
  var date: String = _
  var open: Double = _
  var high: Double = _
  var low: Double = _
  var close: Double = _
  var diff: Double = _
  var percent: Double = _

  def toPrice: Price = Price(date, close)

  //  override def toString: String = f"${date}, ${open}%.2f, ${high}%.2f, ${low}%.2f, ${close}%.2f"
  override def toString: String = f"${date},${close}%.2f"
}

case class Price(date: String, price: Double) {
  override def toString: String = f"${date},${price}%.2f"
}

object Bar {
  def ma(bars: Array[Price], n: Int): Array[Price] = {
    bars.zipWithIndex.map { case (bar, i) =>
      val start = i - n + 1 match {
        case t if t >= 0 => t
        case _ => 0
      }
      val price = (start to i).map(bars(_).price).sum / (i - start + 1)
      Price(bar.date, (price * 10000).toInt / 10000.0)
    }
  }

  def min(prices: Array[Price], n: Int): Array[Price] = {
    prices.zipWithIndex.map { case (bar, i) =>
      val start = i - n + 1 match {
        case t if t >= 0 => t
        case _ => 0
      }
      val (price, idx) = (start to i).map(prices(_).price).zip(start to i).minBy { case (v, _) => v }
      Price(prices(idx).date, price)
    }
  }

  def minx(prices: Array[Price], n: Int): Array[Price] = {
    prices.zipWithIndex.map { case (bar, i) =>
      val end = i + n - 1 match {
        case t if t >= prices.length => prices.length - 1
        case t => t
      }
      val (price, idx) = (i to end).map(prices(_).price).zip(i to end).minBy { case (v, _) => v }
      Price(prices(idx).date, price)
    }
  }

  def max(prices: Array[Price], n: Int): Array[Price] = {
    prices.zipWithIndex.map { case (bar, i) =>
      val start = i - n + 1 match {
        case t if t >= 0 => t
        case _ => 0
      }
      val (price, idx) = (start to i).map(prices(_).price).zip(start to i).maxBy { case (v, _) => v }
      Price(prices(idx).date, price)
    }
  }

  def maxx(prices: Array[Price], n: Int): Array[Price] = {
    prices.zipWithIndex.map { case (bar, i) =>
      val end = i + n - 1 match {
        case t if t >= prices.length => prices.length - 1
        case t => t
      }
      val (price, idx) = (i to end).map(prices(_).price).zip(i to end).maxBy { case (v, _) => v }
      Price(prices(idx).date, price)
    }
  }

  def v(prices: Array[Price]): Array[Int] = {
    prices.zipWithIndex.filter { case (p, idx) =>
      idx > 0 && idx + 1 < prices.length &&
        prices(idx - 1).price >= p.price &&
        prices(idx + 1).price >= p.price
    }.map(_._2)
  }

  def ^(prices: Array[Price]): Array[Int] = {
    prices.zipWithIndex.filter { case (p, idx) =>
      idx > 0 && idx + 1 < prices.length &&
        prices(idx - 1).price <= p.price &&
        prices(idx + 1).price <= p.price
    }.map(_._2)
  }
}

object Store {
  val cookie = "SUV=1712312255354743; vjuids=-69d27717.160f48d85e7.0.0019abdc8a4f; gidinf=x099980109ee0d7c33ae3e458000d078cd4448b5a44b; beans_mz_userid=T1fVe08TKg37; vjlast=1515931404.1520602056.12; mut=zz.go.smuid; _smuid=ALRTtZe2bXDcTduYsUFUI1; _smuid_type=2; IPLOC=CN4100; beans_dmp=%7B%22admaster%22%3A1532783425%2C%22shunfei%22%3A1532783425%2C%22reachmax%22%3A1532783425%2C%22lingji%22%3A1532783425%2C%22yoyi%22%3A1532783425%2C%22ipinyou%22%3A1532783425%2C%22ipinyou_admaster%22%3A1532783425%2C%22miaozhen%22%3A1532783425%2C%22diantong%22%3A1532783425%2C%22huayang%22%3A1532783425%7D; t=1532783423591"

  def nowDate: String = {
    new SimpleDateFormat("yyyyMMdd").format(new Date)
  }

  def fetch(code: String = "zs_000001", start: String = "20000101", end: String = nowDate): Array[Bar] = {
    val client = new HttpClient
    //    val start = "20000101"
    //    val end = "20180804"
    //    val stock = "cn_600029"
    //    val stock = "zs_000016"
    //    val code = "zs_000001"
    //    val url = "http://q.stock.sohu.com/hisHq?code=zs_000001&start=20000504&end=20151215&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp&r=0.8391495715053367&0.9677250558488026"
    val url = s"http://q.stock.sohu.com/hisHq?code=$code&start=$start&end=$end&stat=1&order=D&period=d&rt=json&r=0.8391495715053367&0.9677250558488026"
    client.setCookieString(cookie)
    val body = client.httpGet(url).getBody
    val jo = JSON.parse(body)
    jo.asArr().getObj(0).getArr("hq").array.map(_.asArr().array).map(arr => {
      val bar = new Bar
      //      val sdf = new SimpleDateFormat("yyyy-MM-dd")
      bar.date = arr(0).asStr() //sdf.parse(arr(0).asStr())
      bar.open = arr(1).asStr().toDouble
      bar.close = arr(2).asStr().toDouble
      bar.diff = arr(3).asStr().toDouble
      bar.percent = arr(4).asStr().replace("%", "").toDouble
      bar.low = arr(5).asStr().toDouble
      bar.high = arr(6).asStr().toDouble
      bar
    }).sortBy(_.date)
  }

  def main(args: Array[String]): Unit = {
    val os = new FileOutputStream("C:/users/yml/desktop/stock.csv")
    val bars = fetch(code = "cn_000001")
    val prices = bars.map(_.toPrice)
    //    val min125 = Bar.min(prices, 125)
    //    val minx125 = Bar.minx(prices, 125)
    //    val max125 = Bar.max(prices, 125)
    //    val maxx125 = Bar.maxx(prices, 125)
    val min250 = Bar.minx(prices, 250)
    val max250 = Bar.maxx(prices, 250)
    val ma250 = Bar.ma(prices, 250)
    Bar.v(ma250).foreach(i => {
      val ma = ma250(i)
      //      val m1 = max125(i)
      //      val m2 = maxx125(i)
      //      val m = m1.price > m2.price match {
      //        case true => m1
      //        case false => m2
      //      }
      //      val n1 = min125(i)
      //      val n2 = minx125(i)
      //      val n = n1.price < n2.price match {
      //        case true => n1
      //        case false => n2
      //      }
      val m = max250(i)
      val n = min250(i)
      val r = ((m.price - ma.price) / ma.price * 100).toInt
      //      println(s"${ma.date},${r}")
      val line = s"${prices(i)},${ma.price},${n.price},${m.price},${r}"
      println(line)
      //      os.write(line.getBytes())
      //      os.write("\r\n".getBytes())
    })
    //    val ma250 = Bar.ma(prices, 250)
    //    val min250 = Bar.min(prices, 250)
    //    val minx250 = Bar.minx(prices, 250)
    //    ma250.zipWithIndex.foreach { case (ma, idx) =>
    //      val bar = bars(idx)
    //      val min = min250(idx)
    //      val minx = minx250(idx)
    //      if (idx > 0 && idx + 1 < ma250.length) {
    //        if (ma250(idx - 1).price > ma.price && ma250(idx + 1).price > ma.price) {
    //          println("-", ma, prices, min, minx)
    //        }
    //      }
    //      if (bar.open < ma.price && bar.close > ma.price) {
    //        println("x", ma, prices, min, minx)
    //      }
    //    }
  }
}

