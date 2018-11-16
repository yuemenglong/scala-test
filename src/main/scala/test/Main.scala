package test

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

import scala.collection.immutable.TreeMap
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
}

class RedisProtocal {
  def handle(buf: ByteBuffer): ByteBuffer = {
    null
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
      channel.onRead((buffer: ByteBuffer) => {
        buffer.array().take(buffer.limit()).map(_.toChar).foreach(print)
        //        serverCh.doWrite(buffer)
        channel.doWrite(ByteBuffer.wrap("*0\r\n".getBytes()))
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
