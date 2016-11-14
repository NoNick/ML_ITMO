package ru.ifmo.ctddev.akhimulya.kernels

import ru.ifmo.ctddev.akhimulya.BinaryFunctional
import ru.ifmo.ctddev.akhimulya.Main._

object IdentityKernel extends BinaryFunctional {
    override def apply = (xs: Point, ys: Point) => xs.zip(ys).map(p => p._1 * p._2).sum
    override def getName = "Identity transform"
}
