package test

import org.apache.spark.{SparkConf, SparkContext}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local[2]").setAppName("test")
    val sc = new SparkContext(conf)
    val r1 = sc.parallelize(1 to 100, 2).map(i => (i, i))
    val r2 = sc.parallelize(1 to 100, 3).map(i => (i, i))
    val r3 = r1.join(r2)
    r3.map { case (k, (v1, v2)) => (k, v1 * v2) }.foreach(println)
  }
}
