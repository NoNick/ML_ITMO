import scala.io.Source
import org.sameersingh.scalaplot.Implicits._

import Plotter._

import scala.util.Random

object Main {
    type Point = (Double, Double)
    type MarkedDataSet = Seq[(Point, Int)]  // (point, # of class)

    def main(args: Array[String]): Unit = {
        var data = new Random().shuffle(load("chips.txt"))
        showDataset(data, "source")

        val controlSize = data.size / 5
        val controlSet = data.take(controlSize)
        data = data.drop(controlSize)

        val range = 3 to 15
        showLines(range, LeaveOneOut.costsByParameters(range, new kNN(Metrics.Euclidean),
            data, LeaveOneOut.accuracy), "LOO.noTransform.Euclidean.accuracy")
        showLines(range, LeaveOneOut.costsByParameters(range, new kNN(Metrics.Manhattan),
            data, LeaveOneOut.accuracy), "LOO.noTransform.Manhattan.accuracy")
        showLines(range, CrossValidation.costsByParameters(range, new kNN(Metrics.Euclidean),
            data, CrossValidation.accuracy), "CV.noTransform.Euclidean.accuracy")
        showLines(range, CrossValidation.costsByParameters(range, new kNN(Metrics.Manhattan),
            data, CrossValidation.accuracy), "CV.noTransform.Manhattan.accuracy")

        val k = 9
        showClassifiedDataset(controlSet,
            new kNN(Metrics.Euclidean).setParam(k).train(data).classify(controlSet.map(_._1)),
            "ClassifiedControl.Euclidean.9")
    }

    def euclidMetric(p1: Point, p2: Point) : Double = math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

    /**
     * Loads dataset from file.
     *
     * @param path input file
     * @return (positive, negative) samples
     */
    def load(path: String): MarkedDataSet =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(",")
            ((words(0).toDouble, words(1).toDouble), words(2).toInt)
        }.get.toSeq

    def toPair(a: Iterator[Array[Double]]): Iterator[Point] = a.map(a => (a(0), a(1)))
}
