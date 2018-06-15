package pcs

import java.io._
import java.nio.file.{Path, Paths}

import scala.collection.mutable.ArrayBuffer
import java.io.FileOutputStream
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.commons.io.FileUtils

object PCS {
  val pcs = "D:/pcs/BaiduPCS-Go"

  def exec(cmd: String): Array[String] = {
    val s = s"${pcs} ${cmd}"
    println(s)
    val proc = Runtime.getRuntime.exec(s)
    val br = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val ab = new ArrayBuffer[String]()
    Stream.continually({
      br.readLine()
    }).takeWhile(_ != null).foreach(s => {
      println(s)
      ab += s
    })
    proc.waitFor()
    ab.toArray
  }

  def ls(dir: String): Array[String] = {
    exec(s"ls ${dir} -time -desc").drop(4).filter(s => {
      s.trim.split("""\s+""")(0).matches("""\d+""")
    }).map(s => {
      val name = s.trim.split("""\s+""").last
      s"${dir}/${name}"
    })
  }

  def download(path: String): Array[String] = exec(s"download ${path}")
}

case class ModItem(id: Long, name: String, thumb: String = null, ppath: String = null)

case class Mod(root: String, mod: String, list: String, items: Array[ModItem]) {
  require(items.length > 0)
  println(s"Find Mod ${name}")

  def name: String = {
    val item = items.minBy(_.id)
    val ty = new File(list).getParentFile.getName
    s"[${ty}][${item.id}]_${item.name}"
  }

  def pickTo(dir: String): Unit = {
    if (!new File(dir).isDirectory) {
      throw new Exception(s"[${dir}] Not Dir")
    }
    val zipFile = Paths.get(dir, name + ".zip").toFile
    if (zipFile.exists()) {
      println(s"${zipFile} Exists")
//      return
    }
    val out = new ZipOutputStream(new FileOutputStream(zipFile))
    // mod, list, thumbs
    zip(root, mod, out)
    zip(root, list, out)
    items.foreach(item => {
      if (item.thumb != null) {
        zip(root, item.thumb, out)
      }
    })
    out.close()
    println(s"Pick ${name} Succ")
  }

  def zip(root: String, file: String, out: ZipOutputStream): Unit = {
    val rel = Paths.get(root).relativize(Paths.get(file)).toString
    println(rel)
    out.putNextEntry(new ZipEntry(rel))
    pipe(new FileInputStream(file), out)
  }

  def pipe(is: InputStream, os: OutputStream): Unit = {
    val buf = new Array[Byte](1024 * 4)
    Stream.continually(is.read(buf)).takeWhile(_ >= 0).foreach(len => {
      os.write(buf, 0, len)
      os.flush()
    })
  }
}

object Main {
  def pick(from: String, to: String): Unit = {
    val list = Paths.get(from, "abdata/list").toFile
    if (!list.isDirectory) {
      throw new Exception(s"[${list.getAbsolutePath}] Not Dir")
    }
    val thumb = Paths.get(from, "abdata/thumnbnail_R").toFile
    if (!thumb.isDirectory) {
      throw new Exception(s"[${thumb}] Not Dir")
    }
    if (!Paths.get(to).toFile.isDirectory) {
      throw new Exception(s"[${to}] Not Dir")
    }
    val thumbMap = thumb.listFiles().map(f => {
      val name = f.getName.split("""\.""")(0)
      (name, f.getAbsolutePath)
    }).toMap
    val lists = list.listFiles().filter(_.isDirectory).flatMap(sub => {
      sub.listFiles().filter(_.getName.endsWith("Mlist.txt"))
    })
    val mods = lists.map(list => {
      val br = new BufferedReader(new InputStreamReader(new FileInputStream(list)))
      val lines = Stream.continually(br.readLine()).takeWhile(_ != null).filter(_.length > 0).toArray
      require(lines(1).startsWith("ID"))
      val relPath = lines(0).filter(_.toInt != 65279)
      val mod = Paths.get(from, "abdata", relPath).toFile
      if (!mod.isFile) {
        throw new Exception(s"[${mod}] Not File")
      }
      val items = lines.drop(2).map(line => {
        val sps = line.trim.split("""\t""")
        val id = sps(0).toLong
        val name = sps(1).trim
        val thumb = thumbMap.getOrElse(name, null)
        ModItem(id, name, thumb)
      })
      Mod(from, mod.getAbsolutePath, list.getAbsolutePath, items)
    })
    mods.foreach(m => {
      m.pickTo(from)
    })
  }

  def main(args: Array[String]): Unit = {
    val p = "C:\\Users\\yml\\Desktop\\playhome"
    pick(p, p)
    //    val dir = "/pcs"
    //    val newest = PCS.exec(s"ls ${dir} -time -desc").slice(4, 5)
    //    PCS.download("/pcs/PH3.0/ -p 1000")
    //    PCS.ls("/pcs")
  }
}
