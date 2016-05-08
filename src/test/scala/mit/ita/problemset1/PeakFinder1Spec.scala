package mit.ita.problemset1

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, WordSpec}

class PeakFinder1Spec extends PeakFinderBase {
  val peakFinder: PeakFinder = PeakFinder1
}
