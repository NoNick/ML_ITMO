package ru.ifmo.ctddev.akhimulya.kernels

import ru.ifmo.ctddev.akhimulya.BinaryFunctional
import ru.ifmo.ctddev.akhimulya.Main._

class DistanceKernel(points: Seq[Point]) extends BinaryFunctional {
    val center = points.transpose.map(list => list.sum / list.length)
    def dist(point: Point): Double = math.sqrt(center.zip(point).map(p => math.pow(p._1 - p._2, 2)).sum)
    val avgDist = points.map(dist).sum / points.size

//    override def apply = (a: Point, b: Point) => (avgDist - dist(a)) * (avgDist - dist(b))
    override def apply = (a: Point, b: Point) => dist(a) * dist(b)
    override def getName = "Point to the distance to center transform"
}
