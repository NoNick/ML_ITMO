import Main._

object Utils {
    def F1(source: MarkedDataSet, evaluated: MarkedDataSet): Double = {
        val sets = countTrueFalse(source, evaluated)
        val truePositives = sets._1
        val falsePositives = sets._2
        val falseNegatives = sets._4
        truePositives.toDouble / (2 * truePositives + falsePositives + falseNegatives + 1)
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
}
