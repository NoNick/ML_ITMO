package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

/**
  * Created by noname on 11/14/16.
  */
object Utils {
    def F1(source: MarkedDataSet, evaluated: MarkedDataSet): Double = {
        val sets = countTrueFalse(source, evaluated)
        val truePositives = sets._1
        val falsePositives = sets._2
        val falseNegatives = sets._4
        2 * truePositives.toDouble / (2 * truePositives + falsePositives + falseNegatives + 1)
    }

    def accuracy(source: MarkedDataSet, evaluated: MarkedDataSet): Double =
        source.zip(evaluated).count(pair => pair._1._2 == pair._2._2).toDouble / source.length

    /**
      * Prints matrix
      *
      * True Positive  | False Negative
      * -------------------------------
      * False Positive | True Negative
      *
      * @param source points with correct classes
      * @param evaluated points with classes to evaluate
      */
    def printConfusionMatrix(source: MarkedDataSet, evaluated: MarkedDataSet): Unit = {
        val confusion = splitTrueFalse(source, evaluated)
        System.out.println(confusion._1.size + "\t\t" + confusion._4.size)
        System.out.println(confusion._2.size + "\t\t" + confusion._3.size)
    }

    /**
      * Splits dataset by binary classifier's hit/miss.
      * Points in source and evaluated should be the same.
      *
      * @param source points with correct classes
      * @param evaluated points with classes to evaluate
      * @return (truePositives, falsePositives, trueNegatives, falseNegatives)
      */
    def splitTrueFalse(source: MarkedDataSet, evaluated: MarkedDataSet)
    : (Seq[Point], Seq[Point], Seq[Point], Seq[Point]) = {
        val pairs = source.zip(evaluated)
        if (source.length != evaluated.length || pairs.exists(pair => pair._1._1 != pair._2._1))
            throw new IllegalArgumentException("Datasets for F1 are not the same")

        def getPoint(pair: ((Point, Int), (Point, Int))): Point = pair._1._1
        (pairs.filter(pair => pair._2._2 == 1 && pair._1._2 == pair._2._2).map(getPoint),
            pairs.filter(pair => pair._2._2 == 1 && pair._1._2 != pair._2._2).map(getPoint),
            pairs.filter(pair => pair._2._2 == 0 && pair._1._2 == pair._2._2).map(getPoint),
            pairs.filter(pair => pair._2._2 == 0 && pair._1._2 != pair._2._2).map(getPoint))
    }

    def countTrueFalse(source: MarkedDataSet, evaluated: MarkedDataSet): (Int, Int, Int, Int) = {
        val pairs = source.zip(evaluated)
        if (source.length != evaluated.length || pairs.exists(pair => pair._1._1 != pair._2._1))
            throw new IllegalArgumentException("Datasets for F1 are not the same")

        def getPoint(pair: ((Point, Int), (Point, Int))): Point = pair._1._1
        (pairs.count(pair => pair._2._2 == 1 && pair._1._2 == pair._2._2),
            pairs.count(pair => pair._2._2 == 1 && pair._1._2 != pair._2._2),
            pairs.count(pair => pair._2._2 == 0 && pair._1._2 == pair._2._2),
            pairs.count(pair => pair._2._2 == 0 && pair._1._2 != pair._2._2))
    }

    def toTuple2(point: Seq[Double]): (Double, Double) = point match {
        case Seq(a, b) => (a, b)
        case Seq(a, b, _) => (a, b)
    }

    def printWilcoxon(knn: Classifier, svm: Classifier, data: MarkedDataSet): Unit = {
        val folds = 4
        val repeats = 10
        val F1Pairs = CrossValidation.pairStatistics(knn, svm, data, folds, repeats, Utils.F1)
        val n = F1Pairs.size
        val m = n
        val F1s = (F1Pairs.map(_._1).zip(List.fill(n)(false)) ++ F1Pairs.map(_._2).zip(List.fill(n)(true))).sortBy(_._1)
        def sumRanks(label: Boolean): Double = F1s.zipWithIndex.filter(_._1._2 == label).map(_._2).sum

        val difs = F1Pairs.map(p => p._1 - p._2)
        val avgDif = difs.sum / difs.size
        val variance = math.sqrt(difs.map(d => math.pow(d - avgDif, 2)).sum / (n - 1))
        val tValue = avgDif / (variance / math.sqrt(n))
        System.out.println("t-value " + tValue)

        val Rx = sumRanks(false)
        val Ry = sumRanks(true)
        val W = Rx
        System.out.println("W = " + W)

        val alpha = 0.05
        val Fa = 1.960
        val Wc = (W - m * (m + n + 1.0) / 2d) / math.sqrt(m * n * (m + n + 1.0) / 12.0)
        System.out.println("Wc = " + math.abs(Wc) + "  ?>=  " + Fa)

        val Wcx = 0.5 * Wc  * (1d + Math.sqrt((n + n - 2d) / (n + m - 1d - Wc * Wc)))
        val xa = 1.645
        val ya = 2.1448
        System.out.println("Wcx = " + Math.abs(Wcx) + "  ?>=  " + (xa + ya) / 2d)
    }
}
