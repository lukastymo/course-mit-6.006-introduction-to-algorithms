package mit.ita.problemset1

import scala.annotation.tailrec

object PeakFinder2 extends PeakFinder {

  def find(grid: Array[Array[Int]]): Option[Int] = {
    val Rows = grid.length
    val Cols = if (Rows > 0) grid(0).length else 0

    @tailrec
    def find(y: Int, x: Int): Int = {
      val neighbours = Seq(
        (Some(grid(y)(x)), (y, x)),
        if (y + 1 < Rows) (Some(grid(y + 1)(x)), (y + 1, x)) else (None, (y + 1, x)),
        if (x + 1 < Cols) (Some(grid(y)(x + 1)), (y, x + 1)) else (None, (y, x + 1)),
        if (y - 1 >= 0) (Some(grid(y - 1)(x)), (y - 1, x)) else (None, (y - 1, x)),
        if (x - 1 >= 0) (Some(grid(y)(x - 1)), (y, x - 1)) else (None, (y, x - 1))
      ).filter(_._1.isDefined).map(x => (x._1.get, x._2))

      val max = neighbours.maxBy(x => x._1)
      if (max._1 == grid(y)(x)) grid(y)(x)
      else find(max._2._1, max._2._2)
    }

    if (grid.length == 0 || grid(0).length == 0) None
    else Some(find(0, 0))
  }
}
