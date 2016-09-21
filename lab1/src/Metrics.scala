import Main._
import scala.math._

object Metrics {
    // TODO: make MinkowskyImpl partial in a convenient way
    def Minkowski(p: Double): (Point, Point) => Double =
        (a, b) => pow(pow(abs(a._1 - b._1), p) + pow(abs(a._2 - b._2), p), 1/p)

    val Manhattan = Minkowski(1.0)
    val Euclidean = Minkowski(2.0)
}
