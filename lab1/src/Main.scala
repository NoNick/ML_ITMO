import scala.io.Source
import org.sameersingh.scalaplot.Implicits._

import Plotter._

import scala.util.Random

object Main {
    type Point = Seq[Double]
    type MarkedDataSet = Seq[(Point, Int)]  // (point, # of class)

    // Paraboloid params
    val t = 1.0
    val u = 1.0

    val crossProduct = (xs: Seq[Double]) => (ys: Seq[Double]) => xs.zip(ys).map(p => p._1 * p._2).sum

    def KParaboloid(center: Point, t: Double, u: Double): Seq[Double] => Seq[Double] => Double =
        (a: Seq[Double]) => (b: Seq[Double]) => (crossProduct(projectOnParaboloid(center, t, u)(a))
                                                             (projectOnParaboloid(center, t, u)(b)))

    def KDist(center: Point): Seq[Double] => Seq[Double] => Double =
        (a: Seq[Double]) => (b: Seq[Double]) => distTransform(center)(a) * distTransform(center)(b)

    def KDistDeviation(center: Point, avgDist: Double): Seq[Double] => Seq[Double] => Double =
        (a: Seq[Double]) => (b: Seq[Double]) => (avgDist - distTransform(center)(a)) *
                                                (avgDist - distTransform(center)(b))

    def distTransform(center: Point) =
        (point: Point) => math.sqrt(center.zip(point).map(p => math.pow(p._1 - p._2, 2)).sum)

    def main(args: Array[String]): Unit = {
        //var data = new Random().shuffle(load("chips.txt"))
        val data = load("chips.txt")
        showDataset(data, "source")

//        val controlSize = data.size / 7
//        val controlSet = toParaboloidData(data.take(controlSize))
//        data = data.drop(controlSize)
        val paraboloidData = toParaboloidData(data)

        val k = 7
        val knn = new kNN(Euclidean).setParam(k)
        knn.train(paraboloidData)
        val kNNEvaluated = knn.classify(paraboloidData.map(_._1))
        showClassifiedDataset(paraboloidData, kNNEvaluated, "ClassifiedControl.Euclidean.7")
        val knnF1 = Utils.F1(paraboloidData, kNNEvaluated)
        System.out.println("F1 for kNN: " + knnF1)
        System.out.println("Confusion matrix for kNN: ")
        Utils.printConfusionMatrix(paraboloidData, kNNEvaluated)
        System.out.println()

        val svm = new SVM(crossProduct)
        svm.train(data)
        val svmEvaluated = svm.classify(data.map(_._1))
        showClassifiedDataset(data, svmEvaluated, "SVM.Id")
        System.out.println("F1 for SVM.Id: " + Utils.F1(data, svmEvaluated))
        System.out.println("Confusion matrix for SVM.Id: ")
        Utils.printConfusionMatrix(data, svmEvaluated)
        System.out.println()

        val center = data.map(_._1).transpose.map(list => list.sum / list.length)
        val avgDist = data.map(_._1).map(distTransform(center)).sum / data.size.toDouble
        val svmDist = new SVM(KDistDeviation(center, avgDist))
        svmDist.train(data)
        val svmDistEvaluated = svmDist.classify(data.map(_._1))
        showClassifiedDataset(data, svmDistEvaluated, "SVM.Dist")
        val svmDistF1 = Utils.F1(data, svmDistEvaluated)
        System.out.println("F1 for SVM.Dist: " + svmDistF1)
        System.out.println("Confusion matrix for SVM.Dist: ")
        Utils.printConfusionMatrix(data, svmDistEvaluated)
        System.out.println()

        Utils.printWilcoxon(knn, svmDist, data, knnF1, svmDistF1)

        //val metrics = List(Manhattan, Euclidean, Minkowski3)
        //val datasets = List((data, "noTransform"), (toParaboloidData(data), "onParaboloid"))
        //drawValidationPlots(datasets, LeaveOneOut, metrics, LeaveOneOut.accuracy, "accuracy")
        //drawValidationPlots(datasets, LeaveOneOut, metrics, LeaveOneOut.F1, "F1")
        //drawValidationPlots(datasets, CrossValidation, metrics, CrossValidation.accuracy, "accuracy")
        //drawValidationPlots(datasets, CrossValidation, metrics, CrossValidation.F1, "F1")
    }

    def projectOnParaboloid(center: Point, t: Double, u: Double): Point => Point = point => {
        val x = point(0) - center(0)
        val y = point(1) - center(1)
        List(point(0), point(1), t * x + u * y)
    }

    def toParaboloidData(data: MarkedDataSet): MarkedDataSet = {
        val center = data.map(_._1).transpose.map(list => list.sum / list.length)
        data.map(_._1).map(projectOnParaboloid(center, t, u)).zip(data.map(_._2))
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
