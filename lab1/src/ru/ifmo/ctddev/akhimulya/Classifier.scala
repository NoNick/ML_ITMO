package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

/**
  * Created by noname on 11/14/16.
  */
trait Classifier {
    def train(data: MarkedDataSet, silent: Boolean = true): Unit // changes state

    def classify(points: Seq[Point]): MarkedDataSet

    def setParam(param: Double) : Unit // used for validation
}
