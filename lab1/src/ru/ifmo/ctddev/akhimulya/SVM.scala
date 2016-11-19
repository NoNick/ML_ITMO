package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

class SVM(K: BinaryFunctional) extends Classifier {
    // SVM params
    var C = 2.0
    val initLambda = 0.0
    val updatesPerIteration = 300
    val maxStaleIterations = 3
    val eps = 1e-5
    val tolerance = 1e-3

    // learnt data
    var b: Double = 0.0
    var trained: Seq[Element] = null

    class Element(xs: Point, y: Int, l: Double) {
        val x = xs
        val set = y * 2 - 1
        var lambda = l
        def E(): Double = f(x) - set
    }

    def cost(lambda: Seq[Double], data: MarkedDataSet): Double = {
        val ld = lambda.zip(data)
        -lambda.sum + (ld cross ld).map(p => {
            val lambdai = p._1._1
            val xi = p._1._2._1
            val yi = p._1._2._2 * 2 - 1
            val lambdaj = p._2._1
            val xj = p._2._2._1
            val yj = p._2._2._2 * 2 - 1
            lambdai * lambdaj * yi * yj * K.apply(xi, xj)
        }).sum / 2.0
    }

    val random = scala.util.Random

    // true if updated
    def updateLambda(elements: Seq[Element], pairInd: (Int, Int)): Boolean = {
        val ei = elements(pairInd._1)
        val ej = elements(pairInd._2)
        val lambdai_old = ei.lambda
        val lambdaj_old = ej.lambda

        var L = 0.0
        var H = 0.0
        if (ei.set != ej.set) {
            L = math.max(0, ej.lambda - ei.lambda)
            H = math.min(C, C + ej.lambda - ei.lambda)
        } else {
            L = math.max(0, ej.lambda + ei.lambda - C)
            H = math.min(C, ej.lambda + ei.lambda)
        }

        if ((H - L).abs < eps) {
            return false
        }

        val eta = 2 * K.apply(ei.x, ej.x) - K.apply(ei.x, ei.x) - K.apply(ej.x, ej.x)
        if (eta >= -eps) {
            return false
        }

        ej.lambda = math.min(H, math.max(L, ej.lambda - ej.set * (ei.E() - ej.E()) / eta))
        if ((ej.lambda - lambdaj_old).abs < eps) {
            return false
        }
        ei.lambda = ei.lambda + ei.set * ej.set * (lambdaj_old - ej.lambda)

        val b1 = b - ei.E() - ei.set * (ei.lambda - lambdai_old) * K.apply(ei.x, ei.x)
                            - ej.set * (ej.lambda - lambdaj_old) * K.apply(ei.x, ej.x)
        val b2 = b - ei.E() - ei.set * (ei.lambda - lambdai_old) * K.apply(ei.x, ej.x)
                            - ej.set * (ej.lambda - lambdaj_old) * K.apply(ej.x, ej.x)
        b = if (ei.lambda > eps && ei.lambda < C - eps)      b1
            else if (ej.lambda > eps && ej.lambda < C - eps) b2
            else                                             (b1 + b2) / 2.0

        true
    }

    def trainRecursive(elements: Seq[Element], staleIteration: Int = 0): Int = {
        if (staleIteration >= maxStaleIterations) 0
        else {
            val filtered = elements.zipWithIndex.filter(e =>
                (e._1.set * e._1.E() < -tolerance && e._1.lambda < C) ||
                (e._1.set * e._1.E() > tolerance && e._1.lambda > 0)).map(_._2)
            val changed = random.shuffle(filtered cross elements.indices)
                                .take(updatesPerIteration).map(updateLambda(elements, _)).fold(false)(_ | _)
            trainRecursive(elements, if (changed) 0 else staleIteration + 1) + 1
        }
    }

    def train(data: MarkedDataSet, silent: Boolean = true): Unit = {
        trained = data.map(d => new Element(d._1, d._2, initLambda))
        val iterations = trainRecursive(trained)

        if (!silent) {
            System.out.println("Coordinate descent is completed in " + (iterations * updatesPerIteration) + " updates.")
            val lambda = trained.map(_.lambda)
            System.out.println("Cost for resulting lambda " + cost(lambda, data))
            System.out.println("Lambda: " + lambda.mkString(" "))
        }
    }

    def f(x: Point): Double =
        trained.map(d => d.lambda * d.set * K.apply(d.x, x)).sum + b

    def classify(points: Seq[Point]): MarkedDataSet = {
        if (trained == null) throw new IllegalStateException("ru.ifmo.ctddev.akhimulya.SVM was not trained")
        points.par.map(classify).seq
    }

    def classify(point: Point): (Point, Int) = {
        val y = math.signum(f(point))
        if (y < 0) (point, 0) else (point, 1)
    }

    def setParam(param: Double): Unit = C = param
}
