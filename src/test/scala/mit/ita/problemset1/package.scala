package mit.ita

package object problemset1 {

  def isPeak(grid: Array[Array[Int]], potentialPeak: Int): Boolean = {
    if (grid.length == 0 || grid(0).length == 0) false
    else if (grid.length == 1 && grid(0).length == 1) grid(0)(0) == potentialPeak
    else {
      val neighbours = for {
        row <- grid.indices
        col <- grid(0).indices
        if grid(row)(col) == potentialPeak
      } yield {
        Seq(
          if (row - 1 >= 0) Some(grid(row - 1)(col)) else None,
          if (row + 1 < grid.length) Some(grid(row + 1)(col)) else None,
          if (col - 1 >= 0) Some(grid(row)(col - 1)) else None,
          if (col + 1 < grid(0).length) Some(grid(row)(col + 1)) else None
        ).flatten
      }

      neighbours.exists(_.max <= potentialPeak)
    }

  }
}
