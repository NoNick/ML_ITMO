import scala.concurrent.duration.DurationConversions.Classifier
import scala.util.Random

import Main._

object SingleParameterFitter {
    /**
     * Splits dataset into two random disjoint unions folds times.
     * Size of test sets is seq.size / folds.
     *
     * @param dataset dataset to split
     * @param folds number of parts
     * @return iterator to pairs (train, test)
     */
    def splitForValidation(dataset: MarkedDataSet, folds: Int) : Seq[(MarkedDataSet,
        MarkedDataSet)] = {
        val shuffled = (new Random).shuffle(dataset)
        val blockSize = Math.ceil(shuffled.size / folds).toInt
        Stream.from(0).take(folds).map(i => (shuffled.take(i * blockSize) ++ shuffled.drop((i + 1) * blockSize),
            shuffled.drop(i * blockSize).take(blockSize)))
    }

    def costsByParameters(params: Seq[Int], classifier: kNN, data: MarkedDataSet,
                        cost: (MarkedDataSet, MarkedDataSet) => Double) : Seq[Double] = {
        val splitted = splitForValidation(data, data.length)
        def leaveOneOut(param: Int): Double = { // returns cost
            classifier.setParam(param)
            val costs = splitted.map(pair => {
                val evaluated = classifier.train(pair._1).classify(pair._2(0)._1)
                cost(pair._2, List(evaluated))
            })
            costs.sum / costs.length
        }

        params.map(leaveOneOut)
    }

    def F1(source: MarkedDataSet, evaluated: MarkedDataSet): Double = {
        val sets = splitTrueFalse(source, evaluated)
        val truePositives = sets._1.length
        val falsePositives = sets._2.length
        val falseNegatives = sets._4.length
        val precision = truePositives / (truePositives + falsePositives)
        val recall = truePositives / (truePositives + falseNegatives)
        2 * precision * recall / (precision + recall)
    }

    def accuracy(source: MarkedDataSet, evaluated: MarkedDataSet): Double =
        source.zip(evaluated).count(pair => pair._1._2 == pair._2._2) / source.length

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
}
