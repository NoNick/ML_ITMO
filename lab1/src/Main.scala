import scala.io.Source
import org.sameersingh.scalaplot.Implicits._

import Plotter._

import scala.util.Random

object Main {
    type Point = Seq[Double]
    type MarkedDataSet = Seq[(Point, Int)]  // (point, # of class)

    def main(args: Array[String]): Unit = {
        var data = new Random().shuffle(load("chips.txt"))
        showDataset(data, "source")

        val controlSize = data.size / 7
        val controlSet = toParaboloidData(data.take(controlSize))
        data = data.drop(controlSize)
        val paraboloidData = toParaboloidData(data)

        val k = 9
        showClassifiedDataset(controlSet,
            new kNN(Euclidean).setParam(k).train(paraboloidData).classify(controlSet.map(_._1)),
            "ClassifiedControl.Euclidean.9")

        val metrics = List(Manhattan, Euclidean, Minkowski3)
        val datasets = List((data, "noTransform"), (toParaboloidData(data), "onParaboloid"))
        drawValidationPlots(datasets, LeaveOneOut, metrics, LeaveOneOut.accuracy, "accuracy")
        drawValidationPlots(datasets, LeaveOneOut, metrics, LeaveOneOut.F1, "F1")
        drawValidationPlots(datasets, CrossValidation, metrics, CrossValidation.accuracy, "accuracy")
        drawValidationPlots(datasets, CrossValidation, metrics, CrossValidation.F1, "F1")
    }

    def toParaboloidData(data: MarkedDataSet): MarkedDataSet = {
        def projectOnParaboloid(center: Point, t: Double, u: Double): Point => Point =
            point => List(point(0), point(1), t * point(0) * point(0) + u * point(1) * point(1))
        val center = data.map(_._1).transpose.map(list => list.sum / list.length)
        data.map(_._1).map(projectOnParaboloid(center, 1, 1)).zip(data.map(_._2))
    }


    def drawValidationPlots(datasets: Seq[(MarkedDataSet, String)],
                            validator: SingleParameterFitter,
                            metrics: Seq[Metric],
                            cost: (MarkedDataSet, MarkedDataSet) => Double, costName: String): Unit = {
        val range = 1 to 20

        val lines = (metrics cross datasets).map(pair => {
            val metric = pair._1
            val dataset = pair._2
            (range.map(_.toDouble).zip(validator.costsByParameters(range, new kNN(metric), dataset._1, cost)),
                dataset._2 + "." + metric.getName)
        })
        showLines(lines.toSeq, validator.getName + "." + costName)
    }

    /**
     * Loads dataset from file.
     *
     * @param path input file
     * @return (positive, negative) samples
     */
    def load(path: String): MarkedDataSet =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(",")
            (List(words(0).toDouble, words(1).toDouble), words(2).toInt)
        }.get.toSeq

    def toPair(a: Iterator[Array[Double]]): Iterator[Point] = a.map(a => List(a(0), a(1)))

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }
}
