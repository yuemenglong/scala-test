package test

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

import scala.collection.mutable.ArrayBuffer

case class AcceptAttach(fn: Channel => Unit)

case class ReadAttach(channel: Channel)

case class WriteAttach(channel: Channel, buf: ByteBuffer)

case class ConnAttach(channel: Channel, fn: Channel => Unit)

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
        newKey.attach(ReadAttach(ch))
        fn(ch)
      } else if (key.isConnectable) {
        val ConnAttach(ch, fn) = key.attachment()
        val channel = key.channel.asInstanceOf[SocketChannel]
        if (channel.isConnectionPending) {
          channel.finishConnect
        }
        key.attach(ReadAttach(ch))
        key.interestOps(SelectionKey.OP_READ)
        fn(ch)
      } else if (key.isReadable) {
        val ReadAttach(ch) = key.attachment()
        val channel = key.channel().asInstanceOf[SocketChannel]
        val buf = ByteBuffer.allocate(1024)
        channel.read(buf) match {
          case 0 =>
          case n if n < 0 =>
            ch.doClose()
          case n if n > 0 =>
            buf.flip()
            ch.readFn(buf)
        }
      } else if (key.isWritable) {
        val WriteAttach(ch, buf) = key.attachment()
        val channel = key.channel().asInstanceOf[SocketChannel]
        channel.write(buf)
        if (buf.remaining() == 0) {
          key.attach(ReadAttach(ch))
          key.interestOps(SelectionKey.OP_READ)
        }
      }
    }
  }

  def timeEventLoop(): Unit = {
    val now = System.currentTimeMillis()
    val iter = events.entrySet().iterator()
    Stream.continually({
      iter.hasNext match {
        case true => iter.next() match {
          case e if e.getKey <= now => e.getValue
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

class Channel(key: SelectionKey) {

  val channel: SocketChannel = key.channel().asInstanceOf[SocketChannel]

  //noinspection ScalaUnnecessaryParentheses
  var readFn: ByteBuffer => Unit = (_ => {})
  var closeFn: () => Unit = () => {}
  private var _attachment: Object = _

  def onRead(fn: ByteBuffer => Unit): Unit = {
    readFn = fn
  }

  def doWrite(buf: ByteBuffer): Unit = {
    channel.write(buf)
    if (buf.remaining() != 0) {
      key.attach(WriteAttach(this, buf))
      key.interestOps(SelectionKey.OP_WRITE)
    } else {
      key.attach(ReadAttach(this))
      key.interestOps(SelectionKey.OP_READ)
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

class ProtoReader {

  object ReaderState extends Enumeration {
    type ReadState = Value
    val ArgsCountP, ArgsCount, ArgsCountN,
    ArgDataLenP, ArgDataLen, ArgDataLenN,
    ArgData, ArgDataR, ArgDataN = Value
  }

  import ReaderState._

  var state: ReaderState.Value = ArgsCountP
  var argsCount: Int = 0
  var argLen: Int = 0
  var argData: ArrayBuffer[Array[Byte]] = new ArrayBuffer[Array[Byte]]()

  def read(buf: ByteBuffer): Array[Array[Byte]] = {
    while (buf.remaining() > 0) {
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
      case null => bs.write("*-1\r\n".getBytes())
      case _ =>
        bs.write(s"*${data.length}\r\n".getBytes())
        bs.write(data)
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

class RedisProtocal {

  var buf: ByteBuffer = _
  var reader: ProtoReader = new ProtoReader

  def handle(buffer: ByteBuffer): ByteBuffer = {
    this.buf = this.buf match {
      case null => buffer
      case _ =>
        this.buf.mark()
        this.buf.put(buffer)
        this.buf.reset()
        this.buf
    }
    val bs = new ByteArrayOutputStream()
    Stream.continually({
      reader.read(this.buf)
    }).takeWhile(_ != null).map(args => {
      val cmd = new String(args(0)).toUpperCase()
      val ret: Reply = cmd match {
        case "COMMAND" => command()
        case "GET" => get(new String(args(1)))
        case "SET" => set(new String(args(1)), args(2))
        case "DEL" => del(new String(args(1)))
      }
      ret
    }).foreach(_.getBytes(bs))
    if (this.buf.remaining() == 0) {
      this.buf = null
    }
    ByteBuffer.wrap(bs.toByteArray)
  }

  def command(): ArrayReply = {
    ArrayReply(null)
  }

  def set(key: String, value: Array[Byte]): SimpleStringReply = {
    SimpleStringReply()
  }

  def get(key: String): BulkStringReply = {
    BulkStringReply(null)
  }

  def del(key: String): SimpleStringReply = {
    SimpleStringReply()
  }

}

object Main {

  def main(args: Array[String]): Unit = {
    val nio = new Nio
    var serverCh: Channel = null
    var clientCh: Channel = null
    nio.doConnect(new InetSocketAddress("localhost", 6379), (channel: Channel) => {
      //      println("Conn")
      serverCh = channel
      channel.onRead((buffer: ByteBuffer) => {
        buffer.array().take(buffer.limit()).map(_.toChar).foreach(print)
        clientCh.doWrite(buffer)
      })
    })
    nio.doAccept(new InetSocketAddress(6666), (channel: Channel) => {
      //      println("Accept")
      clientCh = channel
      channel.attach(new RedisProtocal)
      channel.onRead((req: ByteBuffer) => {
        req.array().take(req.limit()).map(_.toChar).foreach(print)
        //        serverCh.doWrite(req)
        val res = channel.attachment().asInstanceOf[RedisProtocal].handle(req)
        res.array().take(res.limit()).map(_.toChar).foreach(print)
        channel.doWrite(res)
      })
    })
    nio.loop()
  }

  //  val socket = new Socket
  //
  //  def handle(channel: SocketChannel, buf: ByteBuffer): ByteBuffer = {
  //    buf.array().take(buf.position()).map(_.toChar).foreach(print)
  //    socket.getOutputStream.write(buf.array().take(buf.position()))
  //    val ret = new Array[Byte](1024 * 1024)
  //    val len = socket.getInputStream.read(ret)
  //    //    ret.take(len).map(_.toChar).foreach(print)
  //    ByteBuffer.wrap(ret, 0, len)
  //  }
  //
  //  def main2(args: Array[String]): Unit = {
  //    socket.connect(new InetSocketAddress("localhost", 6379))
  //    val selector = Selector.open()
  //    val server = ServerSocketChannel.open()
  //    server.configureBlocking(false)
  //    server.bind(new InetSocketAddress(6666))
  //    server.register(selector, SelectionKey.OP_ACCEPT)
  //
  //    while (true) {
  //      selector.select()
  //      val keys = selector.selectedKeys()
  //      val iter = keys.iterator()
  //      while (iter.hasNext) {
  //        val key = iter.next()
  //        iter.remove()
  //        if (key.isAcceptable) {
  //          val channel = server.accept()
  //          channel.configureBlocking(false)
  //          channel.register(selector, SelectionKey.OP_READ)
  //        } else if (key.isReadable) {
  //          val channel = key.channel().asInstanceOf[SocketChannel]
  //          val buf = ByteBuffer.allocate(1024)
  //          channel.read(buf) match {
  //            case 0 =>
  //            case n if n < 0 => channel.close()
  //            case n if n > 0 =>
  //              val ret = handle(channel, buf)
  //              if (ret != null) {
  //                channel.write(ret)
  //                if (buf.remaining() != 0) {
  //                  key.attach(buf)
  //                  key.interestOps(SelectionKey.OP_WRITE)
  //                }
  //              }
  //          }
  //        } else if (key.isWritable) {
  //          val channel = key.channel().asInstanceOf[SocketChannel]
  //          val WriteAttach(ch, buf) = key.attachment()
  //          channel.write(buf)
  //          if (buf.remaining() == 0) {
  //            key.interestOps(SelectionKey.OP_READ)
  //          }
  //        }
  //      }
  //    }
  //  }
}
