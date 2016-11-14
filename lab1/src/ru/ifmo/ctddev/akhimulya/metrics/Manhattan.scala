package ru.ifmo.ctddev.akhimulya.metrics

import ru.ifmo.ctddev.akhimulya.BinaryFunctional

object Manhattan extends BinaryFunctional {
    override def apply = Minkowski(1.0)
    override def getName = "Manhattan"
}
