package ru.ifmo.ctddev.akhimulya.metrics

import ru.ifmo.ctddev.akhimulya.BinaryFunctional

object Euclidean extends BinaryFunctional {
    override def apply = Minkowski(2.0)
    override def getName = "Euclidean"
}
