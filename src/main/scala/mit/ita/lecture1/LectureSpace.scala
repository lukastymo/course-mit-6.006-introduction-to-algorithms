package mit.ita.lecture1

import scala.annotation.tailrec

object FindPeak extends App {
  val SingleArray = Array(1, 3, 4, 5, 12)

  def naive1D(A: Array[Int]): Int = {
    if (A.length <= 1) A(0)
    else {
      if (A.head >= A(1)) 0
      else if (A.last >= A(A.length - 2)) A.length - 1
      else (1 until A.length).find(i => A(i) >= A(i - 1) && A(i) >= A(i + 1)).get
    }
  }

  def findPeak1D(array: Array[Int]): Int = {
    @tailrec
    def findPeak1D(array: Array[Int], left: Int, right: Int): Int = {
      if (left == right) left
      else if (left + 1 == right) {
        if (array(left) >= array(right)) left else right
      } else {
        val m = (left + right) / 2
        if (array(m) >= array(m - 1) && array(m) >= array(m + 1)) {
          m
        } else if (array(m) < array(m - 1)) {
          findPeak1D(array, left, m - 1)
        } else {
          findPeak1D(array, m + 1, right)
        }
      }
    }

    findPeak1D(array, 0, array.length - 1)
  }

  println(SingleArray(naive1D(SingleArray)))
  println(SingleArray(findPeak1D(SingleArray)))

  val A = Array(
    Array(10, 8, 10, 10),
    Array(14, 13, 12, 1),
    Array(15, 9, 11, 21),
    Array(16, 17, 19, 20)
  )

  @tailrec
  def find2DPeakAttempt2(A: Array[Array[Int]], leftCol: Int = 0, rightCol: Int = A(0).length - 1): (Int, Int) = {
    if (leftCol == rightCol) {
      (A.indices.map(i => (i, A(i)(leftCol))).maxBy(_._2)._1, leftCol)
    } else {
      val m = (leftCol + rightCol) / 2
      val globalMaxIdx = A.indices.map(i => (i, A(i)(m))).maxBy(_._2)._1
      if (A(globalMaxIdx)(m) >= A(globalMaxIdx)(m - 1) && A(globalMaxIdx)(m) >= A(globalMaxIdx)(m + 1)) (globalMaxIdx, m)
      else if (A(globalMaxIdx)(m) < A(globalMaxIdx)(m - 1)) {
        find2DPeakAttempt2(A, leftCol, m - 1)
      } else {
        find2DPeakAttempt2(A, m + 1, rightCol)
      }
    }
  }

 }
