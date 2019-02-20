package ml

import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.ml.regression.LinearRegression
import org.apache.spark.sql.SparkSession

case class Rating(userId: Int, movieId: Int, rating: Double, timestamp: Long)

object MLMain {
  val FEATURES = "features"
  val LABEL = "label"
  val PREDICTION = "prediction"

  def main(args: Array[String]): Unit = {
    val session = SparkSession.builder().appName("ML").master("local[*]").getOrCreate()
    import session.implicits._

    val sc = session.sparkContext

    val ratings = sc.textFile("hdfs://localhost:9000/data/ml/ratings.csv").mapPartitionsWithIndex {
      case (0, iter) => iter.drop(1)
      case (_, iter) => iter
    }.map(_.split(",") match {
      case Array(userId, movieId, rating, timestamp) =>
        Rating(userId.toInt, movieId.toInt, rating.toDouble, timestamp.toLong)
    })
    val data = ratings.filter(_.movieId <= 10)
      .groupBy(_.userId).filter { case (_, rs) => rs.size == 10 }
      .map { case (_, arr) =>
        val rs = arr.toArray.sortBy(_.movieId).map(_.rating)
        val label = rs(0)
        val features = Vectors.dense(rs.drop(1))
        (label, features)
      }.collect()
    data.foreach(println)
    val df = sc.parallelize(data).toDF(LABEL, FEATURES)

    //    val df = sc.parallelize(1 to 100).map(i => (i * 1.0, Vectors.dense(Array(i * 1.0, i * 10.0)))).toDF(LABEL, FEATURES)
    //
    val Array(training, test) = df.randomSplit(Array(0.8, 0.2))
    //    training.take(10).foreach(println)

    val lir = new LinearRegression()
      .setFeaturesCol(FEATURES)
      .setLabelCol(LABEL)
      .setRegParam(0.0)
      .setElasticNetParam(0.0)
      .setMaxIter(100)
      .setTol(1e-6)

    val model = lir.fit(training)
    println(model.coefficients)
    println(model.intercept)

    val trans = model.transform(test)
    val predictions = trans.select(PREDICTION).rdd.map(_.getDouble(0))
    val labels = trans.select(LABEL).rdd.map(_.getDouble(0))
    labels.zip(predictions).take(100).foreach(println)
  }
}
