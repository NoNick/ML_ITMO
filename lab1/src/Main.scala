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

        //drawValidationPlots(data, Euclidean, "noTransform")
        //drawValidationPlots(data, Manhattan, "noTransform")
        //drawValidationPlots(data, Minkowski3, "noTransform")
        drawValidationPlots(paraboloidData, Euclidean, "onParaboloid")
        //drawValidationPlots(paraboloidData, Manhattan, "onParaboloid")
        //drawValidationPlots(paraboloidData, Minkowski3, "onParaboloid")
    }

    def toParaboloidData(data: MarkedDataSet): MarkedDataSet = {
        def projectOnParaboloid(center: Point, t: Double, u: Double): Point => Point =
            point => List(point(0), point(1), t * point(0) * point(0) + u * point(1) * point(1))
        val center = data.map(_._1).transpose.map(list => list.sum / list.length)
        data.map(_._1).map(projectOnParaboloid(center, 1, 1)).zip(data.map(_._2))
    }


    def drawValidationPlots(data: MarkedDataSet, metric: Metric, transformName: String): Unit = {
        val range = 3 to 15
        showLines(range, LeaveOneOut.costsByParameters(range, new kNN(metric),
            data, LeaveOneOut.accuracy), "LOO." + transformName + "." + metric.getName + ".accuracy")
        showLines(range, CrossValidation.costsByParameters(range, new kNN(metric),
            data, CrossValidation.accuracy), "CV." + transformName + "." + metric.getName + ".accuracy")
        showLines(range, LeaveOneOut.costsByParameters(range, new kNN(metric),
            data, LeaveOneOut.F1), "LOO." + transformName + "." + metric.getName + ".F1")
        showLines(range, CrossValidation.costsByParameters(range, new kNN(metric),
            data, CrossValidation.F1), "CV." + transformName + "." + metric.getName + ".F1")
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
}
