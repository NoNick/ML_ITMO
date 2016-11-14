import scala.util.Random

import Main._

trait SingleParameterFitter {
    def getName: String

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
