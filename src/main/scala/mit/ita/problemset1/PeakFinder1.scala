package mit.ita.problemset1

import scala.annotation.tailrec

object PeakFinder1 extends PeakFinder {

  def find(grid: Array[Array[Int]]): Option[Int] = {
    val Rows = grid.length
    val Cols = if (Rows == 0) 0 else grid(0).length

    if (Rows == 0 || Cols == 0) None
    else {
      @tailrec
      def findSub(colStart: Int, colEnd: Int): Int = {
        val midCol = (colEnd + colStart) / 2
        val maxRowIndex = findMaxRowIndex(midCol)
        if (colStart == colEnd) grid(maxRowIndex)(midCol)
        else {
          val improved = betterNeighbour(maxRowIndex, midCol, colStart, colEnd)
          if (improved._1 == midCol) grid(maxRowIndex)(midCol)
          else if (improved._1 < midCol) findSub(colStart, improved._1)
          else findSub(improved._1, colEnd)
        }
      }

      def findMaxRowIndex(col: Int): Int = {
        (0 until Rows).map(grid(_)(col)).zipWithIndex.maxBy(_._1)._2
      }

      def betterNeighbour(row: Int, col: Int, minCol: Int, maxCol: Int): (Int, Int) = {
        val neighbours = (
          Some((col, grid(row)(col))) ::
            (if (col - 1 >= minCol) Some((col - 1, grid(row)(col - 1))) else None) ::
            (if (col + 1 <= maxCol) Some((col + 1, grid(row)(col + 1))) else None) ::
            Nil
          ).flatten

        neighbours.maxBy(_._2)
      }

      Some(findSub(0, Cols - 1))
    }
  }

}
