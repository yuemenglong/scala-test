package test


import java.io._

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.orm.Orm
import io.github.yuemenglong.orm.lang.anno.{Entity, Id}
import io.github.yuemenglong.orm.lang.types.Types.DateTime

import sun.misc.Unsafe

@Entity
class Pusher {
  @Id
  var id: Integer = _
  var province: String = _
  var city: String = _
  var country: String = _
  var site: String = _
  var site_code: String = _
  var site_operation_id: String = _
  var device_name: String = _
  var device_vender: String = _
  var device_model: String = _
  var monitoring_device: String = _
  var monitoring_point: String = _
  var date: DateTime = _
  var measured_value: String = _
  var unit: String = _
  var import_time: DateTime = _
  var data_source: String = _
}

object Main {


  private val U = getUnsafeInstance

  //使用方法
  @throws[SecurityException]
  @throws[NoSuchFieldException]
  @throws[IllegalArgumentException]
  @throws[IllegalAccessException]
  private def getUnsafeInstance: Unsafe = {
    val theUnsafeInstance = classOf[Unsafe].getDeclaredField("theUnsafe")
    theUnsafeInstance.setAccessible(true)
    theUnsafeInstance.get(classOf[Unsafe]).asInstanceOf[Unsafe]
  }

  def main(args: Array[String]): Unit = {
    val mem: Long = U.allocateMemory(1024 * 1024 * 1024)
    (0 to 1024 * 1024 * 256).foreach(i => {
      U.putInt(mem + i * 4, 1)
    })
    Thread.sleep(10000)
  }
}
