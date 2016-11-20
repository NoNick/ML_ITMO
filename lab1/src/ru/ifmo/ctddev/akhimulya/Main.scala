package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Plotter._
import ru.ifmo.ctddev.akhimulya.metrics._
import ru.ifmo.ctddev.akhimulya.kernels._

import scala.io.Source

/**
  * Created by noname on 11/14/16.
  */
object Main {
    type Point = Seq[Double]
    type MarkedDataSet = Seq[(Point, Int)]  // (point, # of class)

    // Paraboloid params
    val t = 1.0
    val u = 1.0

    def main(args: Array[String]): Unit = {
        //var data = new Random().shuffle(load("chips.txt"))
        val data = load("chips.txt")
        showDataset(data, "source")

//        val controlSize = data.size / 7
//        val controlSet = toParaboloidData(data.take(controlSize))
//        data = data.drop(controlSize)
        val paraboloidData = toParaboloidData(data)

        val distanceKernel = new DistanceKernel(data.map(_._1))

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

        val svm = new SVM(IdentityKernel)
        svm.train(data, silent = false)
        val svmEvaluated = svm.classify(data.map(_._1))
        showClassifiedDataset(data, svmEvaluated, "SVM.Id")
        System.out.println("F1 for SVM.Id: " + Utils.F1(data, svmEvaluated))
        System.out.println("Confusion matrix for SVM.Id: ")
        Utils.printConfusionMatrix(data, svmEvaluated)
        System.out.println()

        val svmDist = new SVM(distanceKernel)
        svmDist.train(data, silent = false)
        val svmDistEvaluated = svmDist.classify(data.map(_._1))
        showClassifiedDataset(data, svmDistEvaluated, "SVM.Dist")
        val svmDistF1 = Utils.F1(data, svmDistEvaluated)
        System.out.println("F1 for SVM.Dist: " + svmDistF1)
        System.out.println("Confusion matrix for SVM.Dist: ")
        Utils.printConfusionMatrix(data, svmDistEvaluated)
        System.out.println()

        val svmGauss = new SVM(GaussKernel)
        svmGauss.train(data, silent = false)
        val svmGaussEvaluated = svmGauss.classify(data.map(_._1))
        showClassifiedDataset(data, svmGaussEvaluated, "SVM.Gauss")
        val svmGaussF1 = Utils.F1(data, svmGaussEvaluated)
        System.out.println("F1 for SVM.Gauss: " + svmGaussF1)
        System.out.println("Confusion matrix for SVM.Gauss: ")
        Utils.printConfusionMatrix(data, svmGaussEvaluated)
        System.out.println()

//        Utils.compareClassifiers(svmDist, svmGauss, data)
        Utils.compareClassifiers(knn, svmDist, data)

//        val datasetsKNN = List((data, "noTransform"), (toParaboloidData(data), "onParaboloid"))
//        val metrics = List[BinaryFunctional](Manhattan, Euclidean, Minkowski3)
//        val buildKNN = (metric: BinaryFunctional) => new kNN(metric)
//        val rangeKNN = Range.Double(1, 20, 1)
//        drawValidationPlots(datasetsKNN, CrossValidation, buildKNN, metrics, Utils.accuracy, "kNN.accuracy", rangeKNN)
//        drawValidationPlots(datasetsKNN, CrossValidation, buildKNN, metrics, Utils.F1, "kNN.F1", rangeKNN)
//        val buildSVM = (kernel: BinaryFunctional) => new SVM(kernel)
//        drawValidationPlots(List((data, "chips.txt")), CrossValidation, buildSVM,
//            List(IdentityKernel, distanceKernel, GaussKernel), Utils.F1, "SVM.F1", Range.Double(0.3, 3.0, 0.1))
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
                            buildClassifier: BinaryFunctional => Classifier,
                            functions: Seq[BinaryFunctional],
                            cost: (MarkedDataSet, MarkedDataSet) => Double,
                            costName: String,
                            range: Seq[Double]): Unit = {
        val lines = (functions cross datasets).par.map(pair => {
            val metric = pair._1
            val dataset = pair._2
            val ret = (range.zip(validator.costsByParameters(range, buildClassifier(metric), dataset._1, cost)),
                dataset._2 + "." + metric.getName)
            System.out.println("Evaluated " + dataset._2 + "." + metric.getName)
            ret
        }).seq
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
        }.toSeq

    def toPair(a: Iterator[Array[Double]]): Iterator[Point] = a.map(a => List(a(0), a(1)))

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }
}
