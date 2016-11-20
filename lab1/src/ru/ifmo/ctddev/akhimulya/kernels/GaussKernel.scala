package ru.ifmo.ctddev.akhimulya.kernels

import ru.ifmo.ctddev.akhimulya.BinaryFunctional
import ru.ifmo.ctddev.akhimulya.Main._
import ru.ifmo.ctddev.akhimulya.metrics.Euclidean

object GaussKernel extends BinaryFunctional {
    val sigmaSquare = 1.0

    override def apply = (a: Point, b: Point) => math.exp(-math.pow(Euclidean.apply(a, b), 2) / (2 *sigmaSquare))
    override def getName = "Gauss function"
}
