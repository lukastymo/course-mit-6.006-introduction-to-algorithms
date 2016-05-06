package mit.ita.problemset1

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, WordSpec}

class PeakFinder1Spec extends WordSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "PeakFinder1" should {
    "return None for empty Array" in {
      val peak = PeakFinder1.find(Array(Array[Int]()))
      peak must be(empty)
    }

    "return the only number when Array is 1x1" in forAll { n: Int =>
      val peak = PeakFinder1.find(Array(Array[Int](n)))
      println(n)
      peak must be(Some(n))
    }

    "return peak for dynamicly generated array" in {
      forAll(Gen.choose(1, 30), Gen.choose(1, 30)) { (rowSize: Int, colSize: Int) =>
        whenever(rowSize >= 1 && colSize >= 1) {
          forAll(Gen.listOfN(rowSize, Gen.listOfN(colSize, Gen.choose(-1000, 1000)))) { grid =>
            val gridArray = grid.map(_.toArray).toArray
            val peak = PeakFinder1.find(gridArray)

            if (grid.nonEmpty && grid(0).nonEmpty) {
              withClue("peak found: " + peak.get) {
                isPeak(gridArray, peak.get) must be(true)
              }
            } else {
              peak must be(empty)
            }
          }
        }
      }
    }
  }

}
