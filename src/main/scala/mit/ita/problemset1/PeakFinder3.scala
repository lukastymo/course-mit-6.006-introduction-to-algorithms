package mit.ita.problemset1

import scala.annotation.tailrec

object PeakFinder3 extends PeakFinder {

  def find(grid: Array[Array[Int]]): Option[Int] = {
    val Rows = grid.length
    val Cols = if (Rows != 0) grid(0).length else 0

    @tailrec
    def find(yStart: Int, yEnd: Int, xStart: Int, xEnd: Int): Int = {
      if (yStart == yEnd) grid(yStart).max
      else if (xStart == xEnd) {
        val line = for {
          i <- yStart to yEnd
        } yield grid(i)(xStart)
        line.max
      } else {
        val middleY = (yStart + yEnd) / 2
        val middleX = (xStart + xEnd) / 2
        val left  = for (i <- xStart to middleX) yield (grid(middleY)(i), (middleY, i))
        val right = for (i <- middleX + 1 to xEnd) yield (grid(middleY)(i), (middleY, i))
        val up    = for (i <- yStart to middleY) yield (grid(i)(middleX), (i, middleX))
        val down  = for (i <- middleY + 1 to yEnd) yield (grid(i)(middleX), (i, middleX))

        val (max, (y, x)) = (left ++ right ++ up ++ down).maxBy(_._1)
        if (x == middleX && y == middleY) grid(y)(x)
        else {
          val neighbours = Seq(
            Some((max, (y, x))),
            if (y - 1 >= yStart) Some(grid(y - 1)(x), (y - 1, x)) else None,
            if (y + 1 <= yEnd) Some(grid(y + 1)(x), (y + 1, x)) else None,
            if (x - 1 >= xStart) Some(grid(y)(x - 1), (y, x - 1)) else None,
            if (x + 1 <= xEnd) Some(grid(y)(x + 1), (y, x + 1)) else None
          ).flatten

          val (newMax, (newY, newX)) = neighbours.maxBy(_._1)
          if (newMax == max) max
          else {
            if (newY < y && newX < middleX) find(yStart, newY, xStart, newX)
            else if (newY < y && newX > middleX) find(yStart, newY, newX, xEnd)
            else if (newX < middleX) find(newY, yEnd, xStart, newX)
            else find(newY, yEnd, newX, xEnd)
          }
        }
      }
    }

    if (Rows == 0 || Cols == 0) None
    else Some(find(0, Rows - 1, 0, Cols - 1))
  }
}
