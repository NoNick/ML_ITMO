package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

/**
  * Created by noname on 11/14/16.
  */
object CrossValidation extends SingleParameterFitter {
    val folds = 5
    val repeats = 3

    override def costsByParameters(params: Seq[Double], classifier: Classifier, data: MarkedDataSet,
                                   cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[Double] = {
        val costsList: Seq[Seq[Double]] = List.fill(repeats)(costsByParametersImpl(params, classifier, data, cost, folds))
        val tr = costsList.transpose
        tr.map(list => list.sum / list.length) // take averages
    }

    override def getName: String = "CV"

    def costsForFolds(classifier: Classifier, data: MarkedDataSet, folds: Int,
                      cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[Double] = {
        def getCost(sets: (MarkedDataSet, MarkedDataSet)): Double = {
            classifier.train(sets._1)
            val evaluated = classifier.classify(sets._2.map(_._1))
            cost(sets._2, evaluated)
        }
        splitForValidation(data, folds).map(getCost)
    }
}
