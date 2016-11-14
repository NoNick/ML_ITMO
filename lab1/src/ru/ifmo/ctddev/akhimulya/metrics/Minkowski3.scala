package ru.ifmo.ctddev.akhimulya.metrics

import ru.ifmo.ctddev.akhimulya.BinaryFunctional

object Minkowski3 extends BinaryFunctional {
    override def apply = Minkowski(3.0)
    override def getName = "Minkowski3"
}
