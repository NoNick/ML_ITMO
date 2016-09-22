import scala.util.Random

import Main._

trait SingleParameterFitter {
    /**
     * Splits dataset into two random disjoint unions folds times.
     * Size of test sets is seq.size / folds.
     *
     * @param dataset dataset to split
     * @param folds number of parts
     * @return iterator to pairs (train, test)
     */
    def splitForValidation(dataset: MarkedDataSet, folds: Int) : Seq[(MarkedDataSet, MarkedDataSet)] = {
        val shuffled = (new Random).shuffle(dataset)
        val blockSize = Math.ceil(shuffled.size / folds).toInt
        Stream.from(0).take(folds).map(i => (shuffled.take(i * blockSize) ++ shuffled.drop((i + 1) * blockSize),
            shuffled.slice(i * blockSize, i * blockSize + blockSize)))
    }

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

    def costsByParameters(params: Seq[Int], classifier: kNN, data: MarkedDataSet,
        cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[Double]

    def costsByParametersImpl(params: Seq[Int], classifier: kNN, data: MarkedDataSet,
                              cost: (MarkedDataSet, MarkedDataSet) => Double, folds: Int) : Seq[Double] = {
        val split = splitForValidation(data, folds)
        def getCost(param: Int): Double = {
            classifier.setParam(param)
            val costs = split.map(pair => {
                val evaluated = classifier.train(pair._1).classify(pair._2.map(_._1))
                cost(pair._2, evaluated)
            })
            costs.sum / costs.length
        }

        params.map(getCost)
    }
}
