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

    def pairStatistics(c1: Classifier, c2: Classifier, data: MarkedDataSet, folds: Int, repeats: Int,
                       cost: (MarkedDataSet, MarkedDataSet) => Double): Seq[(Double, Double)] = {
        def getCost(sets: (MarkedDataSet, MarkedDataSet)): (Double, Double) = {
            c1.train(sets._1)
            c2.train(sets._1)
            val e1 = c1.classify(sets._2.map(_._1))
            val e2 = c2.classify(sets._2.map(_._1))
            (cost(sets._2, e1), cost(sets._2, e2))
        }
        List.fill(repeats)(splitForValidation(data, folds)).flatten.map(getCost)
    }
}
