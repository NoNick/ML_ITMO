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
}
