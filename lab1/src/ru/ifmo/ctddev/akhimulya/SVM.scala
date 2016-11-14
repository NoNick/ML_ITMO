package ru.ifmo.ctddev.akhimulya

import ru.ifmo.ctddev.akhimulya.Main._

class SVM(K: BinaryFunctional) extends Classifier {
    // SVM params
    var C = 2.0 // 0.34 is empirical optima
    val initLambda = C / 2.0
    val minCost = Double.MinValue
    val minDiff = 1e-3
    val updatesPerIteration = 50
    val maxIterations = 50
    val eps = 1e-2

    // learnt data
    var w0: Double = 0.0
    var trainData: MarkedDataSet = null
    var lambda: Seq[Double] = null

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

    // lambda(i) != 0 && lambda(i) != C
    def gNorm(lambda: Seq[Double], data: MarkedDataSet, i: Int): Double = {
        val x = data.zip(lambda).map(p => {
            val lambdaj = p._2
            val xj = p._1._1
            val yj = p._1._2 * 2 - 1 // from {0, 1} to {-1, 1}
            xj.map(_ * lambdaj * yj)
        }).transpose.map(_.sum)

        val g = (data(i)._2 * 2 - 1) * K.apply(x, data(i)._1) - 1 + lambda(i) / (2.0 * C)
        if (lambda(i) < 1e-6) math.min(g, 0) else if (lambda(i) > C - 1e-6) math.max(g, 0) else g
    }

    val random = scala.util.Random
    def updateLambda(lambda: Seq[Double], data: MarkedDataSet): Seq[Double] = {
        val indices = lambda.zipWithIndex.filter(l => l._1 > eps && l._1 < C)
        if (indices.isEmpty)
            return lambda

        val i = random.shuffle(indices).head._2
        val l = lambda(i)
        val l_ = l - gNorm(lambda, data, i) / K.apply(data(i)._1, data(i)._1)
        lambda.updated(i, math.max(0, math.min(C, l_)))
    }

    // 0 <= lambda <= C; sum(lambda_i * y_i, i in 1..n) = 0
    def fitCondition(lambda: Seq[Double], C: Double, data: MarkedDataSet): Seq[Double] = {
        val normalized = lambda.map(l => math.max(0.0, math.min(C, l)))
        val n = lambda.size
        var lSum = data.map(d => d._2 * 2 - 1).zip(normalized).map(p => p._1 * p._2).sum // from {0, 1} to {-1, 1}
        val result = normalized.zip(data).zipWithIndex.map(l => {
            // on step i tries to change i summand by lSum / (n - i) so eventually lSum = 0
            val lambdai = l._1._1
            val yi = l._1._2._2 * 2 - 1
            val i = l._2
            val lambdai_ = math.max(0.0, math.min(C, lambdai - lSum * yi / (n - i).toDouble))
            lSum = lSum + (lambdai_ - lambdai) * yi
            lambdai_
        })

        if (lSum > 1e-6)
            System.err.println("Cannot fit conditions, sum is " + lSum)
        result
    }

    def trainRecursive(data: MarkedDataSet, lambda: Seq[Double], iteration: Int = 0): (Seq[Double], Int) = {
        var lambda_ = lambda
        Stream.from(0).take(updatesPerIteration).foreach(_ => lambda_ = updateLambda(lambda_, data))
//        System.out.println(lambda.mkString("\t"))
//        System.out.println(cost(lambda_, data))
        if (//cost(lambda, data) <= minCost ||
            //    lambda.zip(lambda_).map(p => (p._1 - p._2).abs).max <= minDiff ||
                iteration + 1 >= maxIterations)
            (lambda_, iteration + 1)
        else trainRecursive(data, lambda_, iteration + 1)
    }

    def train(data: MarkedDataSet, silent: Boolean = true): Unit = {
        val trained = trainRecursive(data, List.fill(data.size)(initLambda))
        lambda = trained._1
        trainData = data

//        val filtered = data.zip(lambda).filter(p => p._2 < filterEps || p._2 > C - filterEps)
//        trainData = filtered.map(_._1)
//        lambda = fitCondition(filtered.map(_._2), C, trainData)

        if (!silent) {
            val lSum = data.map(_._2 * 2 - 1).zip(lambda).map(p => p._1 * p._2).sum
            if (lSum.abs > eps) {
                System.err.println("Sum is not zero " + lSum)
                lambda = fitCondition(lambda, C, trainData)
            }
            System.out.println("Coordinate descent is completed in " + (trained._2 * updatesPerIteration) + " updates.")
            System.out.println("Cost for resulting lambda " + cost(lambda, data))
            System.out.println("Lambda: " + lambda.mkString(" "))
        }

        w0 = data.map(pk => data.zip(lambda).map(p => {
            val lambdai = p._2
            val xi = p._1._1
            val yi = p._1._2 * 2 - 1
            lambdai * yi * K.apply(xi, pk._1)
        }).sum - (pk._2 * 2 - 1).toDouble).sum / data.size
    }

    def a(x: Point, lambda: Seq[Double], data: MarkedDataSet): Double =
        data.zip(lambda).map(p => {
            val lambdai = p._2
            val xi = p._1._1
            val yi = p._1._2 * 2 - 1
            lambdai * yi * K.apply(xi, x)
        }).sum - w0

    def classify(points: Seq[Point]): MarkedDataSet = {
        if (trainData == null || lambda == null) throw new IllegalStateException("ru.ifmo.ctddev.akhimulya.SVM was not trained")
        points.par.map(classify).seq
    }

    def classify(point: Point): (Point, Int) = {
        val y = math.signum(a(point, lambda, trainData))
        if (y < 0) (point, 0) else (point, 1)
    }

    def setParam(param: Double): Unit = C = param
}
