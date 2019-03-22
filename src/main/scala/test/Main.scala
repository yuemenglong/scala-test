package test

import org.apache.spark.streaming.dstream.{DStream, ReceiverInputDStream}
import org.apache.spark.streaming.{Seconds, StreamingContext}
import org.apache.spark.{SparkConf, SparkContext}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val ssc = new StreamingContext(conf, Seconds(1));

    // 获得一个DStream负责连接 监听端口:地址
    val lines: DStream[String] = ssc.socketTextStream("", 0);

    // 对每一行数据执行Split操作
    val words = lines.flatMap(_.split(" "));
    // 统计word的数量
    val pairs = words.map(word => (word, 1));
    val wordCounts = pairs.reduceByKey(_ + _);

    // 输出结果
    wordCounts.print();

    ssc.start(); // 开始
    ssc.awaitTermination(); // 计算完毕退出
  }
}
