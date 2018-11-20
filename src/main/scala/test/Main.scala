package test

import java.io.{ByteArrayOutputStream, IOException}
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

case class AcceptAttach(fn: Channel => Unit)

case class ConnAttach(channel: Channel, fn: Channel => Unit)

case class IOAttach(channel: Channel)

//noinspection ConvertExpressionToSAM
class Nio {
  val selector: Selector = Selector.open()
  var thread: Thread = _
  var running = true
  val events = new java.util.TreeMap[Long, () => Unit]()
  val eventCache = new ArrayBuffer[(Long, () => Unit)]

  def stop(): Unit = {
    running = false
  }

  def netEventLoop(): Unit = {
    val keys = selector.selectedKeys()
    val iter = keys.iterator()
    while (iter.hasNext) {
      val key = iter.next()
      iter.remove()
      if (key.isAcceptable) {
        val AcceptAttach(fn) = key.attachment()
        val server = key.channel().asInstanceOf[ServerSocketChannel]
        val channel = server.accept()
        channel.configureBlocking(false)
        val newKey = channel.register(selector, SelectionKey.OP_READ)
        val ch = new Channel(newKey)
        newKey.attach(IOAttach(ch))
        fn(ch)
      } else if (key.isConnectable) {
        val ConnAttach(ch, fn) = key.attachment()
        val channel = key.channel.asInstanceOf[SocketChannel]
        if (channel.isConnectionPending) {
          channel.finishConnect
        }
        key.attach(IOAttach(ch))
        key.interestOps(SelectionKey.OP_READ)
        fn(ch)
      } else if (key.isReadable) {
        val IOAttach(ch) = key.attachment()
        ch.doRead()
      }
      else if (key.isWritable) {
        val IOAttach(ch) = key.attachment()
        ch.doWrite()
      }
    }
  }

  def timeEventLoop(): Unit = {
    val now = System.currentTimeMillis()
    val iter = events.entrySet().iterator()
    Stream.continually({
      iter.hasNext match {
        case true => iter.next() match {
          case e if e.getKey <= now =>
            iter.remove()
            e.getValue
          case _ => null
        }
        case false => null
      }
    }).takeWhile(_ != null).foreach(_ ())
    eventCache.foreach { case (t, fn) => events.put(t, fn) }
    eventCache.clear()
  }

  def loop(): Unit = {
    while (running) {
      selector.select(1)
      netEventLoop()
      timeEventLoop()
    }
  }

  def doAccept(addr: InetSocketAddress, fn: Channel => Unit): Unit = {
    val server = ServerSocketChannel.open()
    server.configureBlocking(false)
    server.bind(addr)
    val key = server.register(selector, SelectionKey.OP_ACCEPT)
    key.attach(AcceptAttach(fn))
  }

  def doConnect(addr: InetSocketAddress, fn: Channel => Unit): Unit = {
    val channel = SocketChannel.open()
    channel.configureBlocking(false)
    channel.connect(addr)
    val key = channel.register(selector, SelectionKey.OP_CONNECT)
    val ch = new Channel(key)
    key.attach(ConnAttach(ch, fn))
  }

  def doTimeout(timeout: Int, fn: () => Unit): Unit = {
    val now = System.currentTimeMillis()
    eventCache += ((now + timeout, fn))
  }
}

class Channel(val key: SelectionKey) {
  final val BUF_SIZE = 4096
  final val BUF_COUNT = 32

  val channel: SocketChannel = key.channel().asInstanceOf[SocketChannel]

  //noinspection ScalaUnnecessaryParentheses
  var readFn: ByteBuffer => Unit = (_ => {})
  var closeFn: () => Unit = () => {}
  private var _attachment: Object = _
  private val _readBuf: ArrayBuffer[ByteBuffer] = new ArrayBuffer[ByteBuffer]()
  private val _writeBuf: util.Deque[ByteBuffer] = new util.ArrayDeque[ByteBuffer]()

  def onRead(fn: ByteBuffer => Unit): Unit = {
    readFn = fn
  }

  def doRead(): Unit = {
    val channel = key.channel().asInstanceOf[SocketChannel]
    val buf = ByteBuffer.allocate(BUF_SIZE)
    try {
      val len = channel.read(buf)
      len match {
        case 0 =>
        case n if n < 0 => throw new IOException("Read <0 And Close")
        case n if n > 0 =>
          buf.flip()
          RedisStatus.readBytes += buf.remaining()
          readFn(buf)
      }
    } catch {
      case _: IOException => doClose()
    }
  }

  def doWrite(): Unit = {
    // 立刻写优化
    var cont = true
    while (cont && _writeBuf.size() > 0) {
      val buf = _writeBuf.peekFirst()
      channel.write(buf)
      if (buf.remaining() == 0) {
        _writeBuf.pollFirst()
      } else {
        cont = false
      }
    }
    if (_writeBuf.size() == 0) {
      key.attach(IOAttach(this))
      key.interestOps(SelectionKey.OP_READ)
    } else {
      key.attach(IOAttach(this))
      key.interestOps(SelectionKey.OP_WRITE | SelectionKey.OP_READ)
    }
  }

  def doWrite(buf: ByteBuffer): Unit = {
    RedisStatus.writeBytes += buf.remaining()
    // 立刻写优化
    if (_writeBuf.size() == 0) {
      channel.write(buf)
      if (buf.remaining() > 0) {
        _writeBuf.addLast(buf)
      }
    } else {
      _writeBuf.addLast(buf)
    }
    if (_writeBuf.size() == 0) {
      key.attach(IOAttach(this))
      key.interestOps(SelectionKey.OP_READ)
    } else {
      key.attach(IOAttach(this))
      key.interestOps(SelectionKey.OP_WRITE | SelectionKey.OP_READ)
    }
  }

  def doClose(): Unit = {
    channel.close()
    key.cancel()
    closeFn()
  }

  def onClose(fn: () => Unit): Unit = {
    closeFn = fn
  }

  def attach(obj: Object): Unit = _attachment = obj

  def attachment(): Object = _attachment
}

class InvalidArgsExecption extends RuntimeException("Invalid Args")

class QuitExecption extends RuntimeException()

class ProtoReader {

  object ReaderState extends Enumeration {
    type ReadState = Value
    val ArgsCountP, ArgsCount, ArgsCountN,
    ArgDataLenP, ArgDataLen, ArgDataLenN,
    ArgData, ArgDataR, ArgDataN = Value
  }

  import ReaderState._

  var buf: ByteBuffer = _
  var state: ReaderState.Value = ArgsCountP
  var argsCount: Int = 0
  var argLen: Int = 0
  var argData: ArrayBuffer[Array[Byte]] = new ArrayBuffer[Array[Byte]]()

  def read(buffer: ByteBuffer): Stream[Array[Array[Byte]]] = {
    buf = buf match {
      case null => buffer
      case _ =>
        val bs = new ByteArrayOutputStream()
        bs.write(buf.array(), buf.position(), buf.remaining())
        bs.write(buffer.array(), buffer.position(), buffer.remaining())
        ByteBuffer.wrap(bs.toByteArray)
    }
    val ret = Stream.continually(proc()).takeWhile(_ != null)
    if (buf.remaining() == 0) {
      buf = null
    }
    ret
  }

  def proc(): Array[Array[Byte]] = {
    while (buf != null && buf.remaining() > 0) {
      state match {
        case ArgsCountP => buf.get() match {
          case '*' => state = ArgsCount
          case _ => throw new InvalidArgsExecption
        }
        case ArgsCount => buf.get() match {
          case '\r' => state = ArgsCountN
          case b => argsCount = argsCount * 10 + b - '0'
        }
        case ArgsCountN => buf.get() match {
          case '\n' => state = ArgDataLenP
          case _ => throw new InvalidArgsExecption
        }
        case ArgDataLenP => buf.get() match {
          case '$' => state = ArgDataLen
          case _ => throw new InvalidArgsExecption
        }
        case ArgDataLen => buf.get() match {
          case '\r' => state = ArgDataLenN
          case b => argLen = argLen * 10 + b - '0'
        }
        case ArgDataLenN => buf.get() match {
          case '\n' => state = ArgData
          case _ => throw new RuntimeException
        }
        case ArgData => buf.remaining() >= argLen match {
          case true =>
            val arg = new Array[Byte](argLen)
            buf.get(arg)
            argLen = 0
            argData += arg
            state = ArgDataR
          case false => return null // 等数据全了在返回
        }
        case ArgDataR => buf.get() match {
          case '\r' => state = ArgDataN
          case _ => throw new RuntimeException
        }
        case ArgDataN => buf.get() match {
          case '\n' => argsCount == argData.length match {
            case false => state = ArgDataLenP //继续读
            case true =>
              argsCount = 0
              state = ArgsCountP
              val ret = argData.toArray
              argData.clear
              return ret
          }
          case _ => throw new RuntimeException
        }
      }
    }
    null
  }
}

trait Reply {
  //noinspection AccessorLikeMethodIsUnit
  def getBytes(bs: ByteArrayOutputStream): Unit
}

case class SimpleStringReply(status: String = "OK") extends Reply {
  override def getBytes(bs: ByteArrayOutputStream): Unit = {
    bs.write(s"+${status}\r\n".getBytes())
  }
}

case class ErrorReply(errType: String = "ERR", errMsg: String) extends Reply {
  override def getBytes(bs: ByteArrayOutputStream): Unit = {
    bs.write(s"-${errType} ${errMsg}\r\n".getBytes())
  }
}

case class IntegerReply(value: Long) extends Reply {
  override def getBytes(bs: ByteArrayOutputStream): Unit = {
    bs.write(s":${value}\r\n".getBytes())
  }
}

case class BulkStringReply(data: Array[Byte]) extends Reply {
  override def getBytes(bs: ByteArrayOutputStream): Unit = {
    data match {
      case null => bs.write("$-1\r\n".getBytes())
      case _ =>
        bs.write(s"$$${data.length}\r\n".getBytes())
        bs.write(data)
        bs.write("\r\n".getBytes())
    }
  }
}

case class ArrayReply(data: Array[Reply]) extends Reply {
  override def getBytes(bs: ByteArrayOutputStream): Unit = {
    data match {
      case null => bs.write("*-1\r\n".getBytes())
      case _ =>
        bs.write(s"*${data.length}\r\n".getBytes())
        data.foreach(_.getBytes(bs))
    }
  }
}

object RedisStatus {
  var readBytes: Long = 0

  var writeBytes: Long = 0

  var proto: RedisProto = _

  var gCh: Channel = _
}

object RedisStore {

  private val map: util.HashMap[String, Object] = new util.HashMap[String, Object]()

  def set(key: String, value: Array[Byte]): Unit = {
    map.put(key, value)
  }

  def get(key: String): Array[Byte] = {
    map.getOrDefault(key, null).asInstanceOf[Array[Byte]]
  }

  def del(key: String): Int = {
    map.remove(key) match {
      case null => 0
      case _ => 1
    }
  }

  def sadd(key: String, value: Array[Byte]): Int = {
    val ret = map.get(key) match {
      case null =>
        val s = new util.HashSet[ByteBuffer]()
        map.put(key, s)
        s.add(ByteBuffer.wrap(value))
      case s: util.HashSet[ByteBuffer] => s.add(ByteBuffer.wrap(value))
      case _ => throw new InvalidArgsExecption
    }
    ret match {
      case true => 1
      case false => 0
    }
  }

  def smember(key: String): Set[Array[Byte]] = {
    map.get(key) match {
      case null => Set()
      case s: util.HashSet[ByteBuffer] => s.toSet[ByteBuffer].map(_.array())
      case _ => throw new InvalidArgsExecption
    }
  }

  def scard(key: String): Long = {
    map.get(key) match {
      case null => 0
      case s: util.HashSet[ByteBuffer] => s.size()
      case _ => throw new InvalidArgsExecption
    }
  }

  def srem(key: String, value: Array[Byte]): Int = {
    val ret = map.get(key) match {
      case null => false
      case s: util.HashSet[ByteBuffer] => s.remove(ByteBuffer.wrap(value))
      case _ => throw new InvalidArgsExecption
    }
    ret match {
      case true => 1
      case false => 0
    }
  }

  def flushAll(): Unit = {
    map.clear()
  }
}

class RedisProto {

  var reader: ProtoReader = new ProtoReader

  def handle(buffer: ByteBuffer): ByteBuffer = {
    //    this.buf = this.buf match {
    //      case null => buffer
    //      case _ =>
    //        val bs = new ByteArrayOutputStream()
    //        bs.write(this.buf.array(), this.buf.position(), this.buf.remaining())
    //        bs.write(buffer.array(), buffer.position(), buffer.remaining())
    //        ByteBuffer.wrap(bs.toByteArray)
    //    }
    val bs = new ByteArrayOutputStream()
    reader.read(buffer).map(args => {
      val cmd = new String(args(0)).toUpperCase()
      val ret: Reply = cmd match {
        case "COMMAND" => command()
        case "SET" => set(new String(args(1)), args(2))
        case "GET" => get(new String(args(1)))
        case "DEL" => del(new String(args(1)))
        case "SADD" => sadd(new String(args(1)), args(2))
        case "SCARD" => scard(new String(args(1)))
        case "SMEMBERS" => smembers(new String(args(1)))
        case "SREM" => srem(new String(args(1)), args(2))
        case "FLUSHALL" => flushAll()
        case "QUIT" => throw new QuitExecption
      }
      ret
    }).foreach(_.getBytes(bs))
    ByteBuffer.wrap(bs.toByteArray)
  }

  def command(): ArrayReply = {
    ArrayReply(null)
  }

  def set(key: String, value: Array[Byte]): SimpleStringReply = {
    RedisStore.set(key, value)
    SimpleStringReply()
  }

  def get(key: String): BulkStringReply = {
    val data = RedisStore.get(key)
    BulkStringReply(data)
  }

  def del(key: String): IntegerReply = {
    val ret = RedisStore.del(key)
    IntegerReply(ret)
  }

  def sadd(key: String, value: Array[Byte]): IntegerReply = {
    val ret = RedisStore.sadd(key, value)
    IntegerReply(ret)
  }

  def smembers(key: String): ArrayReply = {
    val bulks = RedisStore.smember(key).map(BulkStringReply)
    ArrayReply(bulks.toArray)
  }

  def scard(key: String): IntegerReply = {
    val size = RedisStore.scard(key)
    IntegerReply(size)
  }

  def srem(key: String, value: Array[Byte]): IntegerReply = {
    val ret = RedisStore.srem(key, value)
    IntegerReply(ret)
  }

  def flushAll(): SimpleStringReply = {
    RedisStore.flushAll()
    SimpleStringReply()
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val nio = new Nio
    var serverCh: Channel = null
    var clientCh: Channel = null
    nio.doConnect(new InetSocketAddress("localhost", 6379), (channel: Channel) => {
      serverCh = channel
      channel.onRead((buffer: ByteBuffer) => {
        clientCh.doWrite(buffer)
        //        println(">>>>")
        //        buffer.array().take(buffer.limit()).map(_.toChar).foreach(print)
      })
    })
    nio.doAccept(new InetSocketAddress(6666), (channel: Channel) => {
      RedisStatus.gCh = channel
      clientCh = channel
      channel.attach(new RedisProto)
      RedisStatus.proto = channel.attachment().asInstanceOf[RedisProto]
      channel.onRead((req: ByteBuffer) => {
        //        println("<<<<")
        //        req.array().take(req.limit()).map(_.toChar).foreach(print)
        //        req.array().take(req.limit()).map(_.toInt).foreach(os.write)
        try {
          //          serverCh.doWrite(req)
          val res = channel.attachment().asInstanceOf[RedisProto].handle(req)
          channel.doWrite(res)
          //          println(">>>>")
          //          res.array().take(res.limit()).map(_.toChar).foreach(print)
        } catch {
          case _: QuitExecption => channel.doClose()
        }
      })
    })
    nio.loop()
  }
}
