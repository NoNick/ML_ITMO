import Main._
import scala.math._

trait Metric {
    def Minkowski(p: Double): (Point, Point) => Double =
        (a, b) => pow(a.zip(b).map(pair => math.pow(math.abs(pair._1 - pair._2), p)).sum, 1/p)

    def getName: String
    def dist: (Point, Point) => Double
}

object Manhattan extends Metric {
    override def dist = Minkowski(1.0)
    override def getName = "Manhattan"
}

object Euclidean extends Metric {
    override def dist = Minkowski(2.0)
    override def getName = "Euclidean"
}

object Minkowski3 extends Metric {
    override def dist = Minkowski(3.0)
    override def getName = "Minkowski3"
}