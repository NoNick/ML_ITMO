import Main._

class SVM(K: Seq[Double] => Seq[Double] => Double) {
    // SVM params
    val C = 0.34
    val initLambda = C / 2.0
    val descendEps = 1e-1
    val filterEps = 1e-2

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
            lambdai * lambdaj * yi * yj * K(xi)(xj)
        }).sum / 2.0
    }

    def gNorm(lambda: Seq[Double], data: MarkedDataSet, i: Int): Double = {
        val x = data.zip(lambda).map(p => {
            val lambdaj = p._2
            val xj = p._1._1
            val yj = p._1._2 * 2 - 1 // from {0, 1} to {-1, 1}
            xj.map(_ * lambdaj * yj)
        }).transpose.map(_.sum)

        val g = -1 + K(x)(data(i)._1) + lambda(i) / (2.0 * C)
        if (lambda(i) < 1e-6) math.min(g, 0) else if (lambda(i) > C - 1e-6) math.max(g, 0) else g
    }

    def updateLambda(lambdaIn: Seq[Double], data: MarkedDataSet): Seq[Double] = {
        var lambda: List[Double] = lambdaIn.toList
        lambda.zipWithIndex.foreach(p => {
            val i = p._2
            val l = p._1
            val l_ = l - gNorm(lambda, data, i) / K(data(i)._1)(data(i)._1)
            lambda = lambda.updated(i, math.max(0, math.min(C, l_)))
        })
        lambda
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

    def trainRecursive(data: MarkedDataSet, lambda: Seq[Double]): (Seq[Double], Int) = {
        val lambda_ = updateLambda(lambda, data)
//        System.out.println(lambda.mkString("\t"))
        if (lambda.zip(lambda_).map(p => math.abs(p._1 - p._2)).sum / lambda.size < descendEps) (lambda_, 1)
        else {
            val result = trainRecursive(data, lambda_)
            (result._1, result._2 + 1)
        }
    }

    def train(data: MarkedDataSet): Unit = {
        val trained = trainRecursive(data, List.fill(data.size)(initLambda))
        lambda = trained._1

//        val filtered = data.zip(lambda).filter(p => p._2 < filterEps || p._2 > C - filterEps)
//        trainData = filtered.map(_._1)
//        lambda = fitCondition(filtered.map(_._2), C, trainData)
        lambda = fitCondition(lambda, C, data)
        trainData = data
        System.out.println("Coordinate descent is completed in " + trained._2 + " steps.")
        System.out.println("Lambda: " + lambda.mkString(" "))

        val p0 = data.head
        w0 = data.zip(lambda).map(p => {
            val lambdai = p._2
            val xi = p._1._1
            val yi = p._1._2 * 2 - 1
            lambdai * yi * K(xi)(p0._1)
        }).sum - (p0._2 * 2 - 1).toDouble
    }

    def a(x: Point, lambda: Seq[Double], data: MarkedDataSet): Double =
        data.zip(lambda).map(p => {
            val lambdai = p._2
            val xi = p._1._1
            val yi = p._1._2 * 2 - 1
            lambdai * yi * K(xi)(x)
        }).sum - w0

    def classify(points: Seq[Point]): MarkedDataSet = {
        if (trainData == null || lambda == null) throw new IllegalStateException("SVM was not trained")
        points.par.map(classify).seq
    }

    def classify(point: Point): (Point, Int) = {
        val y = math.signum(a(point, lambda, trainData))
        if (y < 0) (point, 0) else (point, 1)
    }
}
