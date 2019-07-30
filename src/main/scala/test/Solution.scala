package test

import java.util

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x

  override def toString: String = {
    if (next != null) {
      s"${x}, ${next}"
    } else {
      s"${x}"
    }
  }
}

object ListNode {
  def apply(arr: Array[Int]): ListNode = {
    if (arr.length == 0) {
      return null
    }
    val head = new ListNode(arr(0))
    var cur = head
    arr.drop(1).foreach(i => {
      cur.next = new ListNode(i)
      cur = cur.next
    })
    head
  }
}

object Solution {
//  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
//
//    val w = matrix(0).length
//    val h = matrix.length
//
//    def next(x: Int, y: Int, dir: (Int, Int)): Unit = {
//
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    println(Solution
//      .maxSubArray(Array(-1))
//    )
//  }
}