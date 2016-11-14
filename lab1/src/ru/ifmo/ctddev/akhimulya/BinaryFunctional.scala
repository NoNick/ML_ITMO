package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

import scala.math._

trait BinaryFunctional {
    def Minkowski(p: Double): (Point, Point) => Double =
        (a, b) => pow(a.zip(b).map(pair => pow((pair._1 - pair._2).abs, p)).sum, 1/p)

    def getName: String
    def apply: (Point, Point) => Double
}
