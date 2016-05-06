package mit.ita.lecture1

import org.scalatest.{FlatSpec, Matchers, MustMatchers}
import FindPeak._

class FindPeakSpec extends FlatSpec with MustMatchers {

  "Find 1D a peak" should "find a peak" in {
    val findPeaksAlgos = Array(naiveFindPeak1D _, findPeak1D _)

    findPeaksAlgos.foreach(f => f(Array(10, 20, 30)) must be(2))
    findPeaksAlgos.foreach(f => f(Array(10, 10, 5)) must (be (0) or be (1)))
    findPeaksAlgos.foreach(f => f(Array(30, 20, 10)) must be (0))
  }

  "Find 2D a peak" should "find a peak" in {
    val A = Array(Array(10, 11, 11, 12),
      Array(14, 13, 12, 11),
      Array(15,  9, 11, 17),
      Array(16, 17, 19, 20)
    )

    find2DPeakAttempt2(A) must (be (0, 3) or be(3, 3))
  }

}
