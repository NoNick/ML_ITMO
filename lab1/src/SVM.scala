import Main._

class SVM(K: Seq[Double] => Seq[Double] => Double, transform: Seq[Double] => Seq[Double]) {
    val initLambda = 0.5
    val C = 2.0
    val alpha = 0.001
    val iterations = 200
    var w0 = 0.0
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

    // {-1 + 1/2 * sum(lambda_j * y_i * y_j * K(x_i, x_j), j in 1..n) + 1/2 * lambda_i * K(x_i, x_i)}, i in 1..n
//    def grad(lambda: Seq[Double], data: MarkedDataSet): Seq[Double] = {
//        lambda.zip(data).map(pi => -1 + data.zip(lambda).map(pj => {
//            val lambdaj = pj._2
//            val xj = pj._1._1
//            val yj = pj._1._2 * 2 - 1 // from {0, 1} to {-1, 1}
//            val xi = pi._2._1
//            val yi = pi._2._2 * 2 - 1
//            lambdaj * yi * yj * K(xi)(xj)
//        }).sum / 2.0 + pi._1 * K(pi._2._1)(pi._2._1) / 2.0)
//    }

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

        //if (crossProduct(result)(data.map(_._2 * 2 - 1).map(_.toDouble)) > 1e-6)
        if (lSum > 1e-6)
            System.err.println("Cannot fit conditions, sum is " + lSum)
        result
    }

//    def train(data: MarkedDataSet): Seq[Double] = {
//        var lambda = fitCondition(List.fill(data.size)(initLambda), C, data)
//        var ys = data.map(_._2 * 2 - 1).map(_.toDouble)
//        Stream.from(0).take(iterations).foreach(_ => {
//            lambda = lambda.zip(grad(lambda, data)).map(p => p._1 + alpha * p._2)
//            //System.out.println(lambda.mkString(" "))
//            //System.out.println(crossProduct(lambda)(ys))
//            lambda = fitCondition(lambda, C, data)
//            System.out.println(lambda.mkString(" "))
//            System.out.println(cost(lambda, data))
//            //System.out.println(crossProduct(lambda)(ys))
//        })
//        lambda
//    }

    def trainRecursive(data: MarkedDataSet, lambda: Seq[Double]): Seq[Double] = {
        val lambda_ = updateLambda(lambda, data)
        System.out.println(lambda.mkString("\t"))
        if (lambda.zip(lambda_).map(p => math.abs(p._1 - p._2)).max < 1e-3) lambda_
        else trainRecursive(data, lambda_)
    }

    def train(data: MarkedDataSet): Unit = {
        lambda = trainRecursive(data, List.fill(data.size)(initLambda))
        trainData = data
        w0 = data.zip(lambda).map(p => {
            val lambdai = p._2
            val xi = p._1._1
            val yi = p._1._2 * 2 - 1
            lambdai * yi * K(xi)(data(0)._1)
        }).sum - (data(0)._2 * 2 - 1).toDouble
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
        points.map(classify)
    }

    def classify(point: Point): (Point, Int) = {
        val y = math.signum(a(point, lambda, trainData))
        if (y < 0) (point, 0) else (point, 1)
    }
}
