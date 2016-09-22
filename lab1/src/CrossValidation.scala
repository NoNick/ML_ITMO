import Main._

object CrossValidation extends SingleParameterFitter {
    val folds = 5
    val repeats = 100

    override def costsByParameters(params: Seq[Int], classifier: kNN, data: MarkedDataSet,
                                   cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[Double] = {
        val costsList: Seq[Seq[Double]] = List.fill(repeats)(costsByParametersImpl(params, classifier, data, cost, folds))
        val tr = costsList.transpose
        tr.map(list => list.sum / list.length) // take averages
    }

}
