import Main._

object LeaveOneOut extends SingleParameterFitter {
    override def costsByParameters(params: Seq[Int], classifier: kNN, data: MarkedDataSet,
                                   cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[Double] =
        costsByParametersImpl(params, classifier, data, cost, data.length)
}
