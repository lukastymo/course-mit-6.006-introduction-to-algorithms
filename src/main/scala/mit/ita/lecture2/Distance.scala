package mit.ita.lecture2

import mit.ita.tools.MeasureTime._

object Distance extends App {

  private def toMapAndTotalWords(s: String): (Map[String, Int], Long) = {
    def normalize(s: String): Map[String, Int] = {
      s.replaceAll("[^A-Za-z0-9 \n]", " ")
        .split("\\s+")
        .map(_.trim)
        .groupBy(identity)
        .mapValues(_.length)
    }

    val words1Map = time {
      normalize(s)
    }
    val words1Set = words1Map.keySet
    val totalWords1 = words1Map.values.map(x => x * x).map(_.toLong).sum

    (words1Map, totalWords1)
  }

  def countDistance(): Unit = {
    val File1 = "src/main/resources/lecture2/t2.bobsey.txt"
    val File2 = "src/main/resources/lecture2/t3.lewis.txt"

    val lines1 = scala.io.Source.fromFile(File1).mkString
    val lines2 = scala.io.Source.fromFile(File2).mkString
    val (words1Map, totalWords1) = toMapAndTotalWords(lines1)
    val (words2Map, totalWords2) = toMapAndTotalWords(lines2)
    val sum = words1Map.keySet.map { w =>
      words1Map.getOrElse(w, 0) * words2Map.getOrElse(w, 0).toLong
    }.sum

    val distance = sum / Math.sqrt(1.0 * totalWords1 * totalWords2)
    val angle = Math.acos(distance)

    println(words1Map.keySet.size + " uqniue words")
    println(words2Map.keySet.size + " unique words")

    println(s"distance: $distance")
    println(s"radians: $angle")
    println(s"angle: ${180 * angle / Math.PI}")
  }

  time {
    countDistance()
  }
}
