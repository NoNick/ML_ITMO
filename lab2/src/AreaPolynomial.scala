class AreaPolynomial(n: Int) extends Model {
    //val maxParam = 1e5
    //def initParams: Seq[Double] = Stream.from(1).take(n).map(i => math.pow(maxParam, 1.0 / i)).reverse
    //override def params: Seq[Double] = initParamsPoly(n) ++ initParamsPoly(n) ++ initParamsPoly(n)
    
    var params: Seq[Double] = List.fill(n)(1.0)

    def apply(ps: Seq[Double], x: (Double, Double)): Double = {
        ps.zipWithIndex.map(p => p._1 * math.pow(x._1, p._2)).sum
    }

    override def learn(data: Seq[(Double, Double, Double)], optimizer: Optimizer): Unit = {
        val costByArea = data.map(x => (x._1, x._3))
        val initParams = Stream.from(1).take(n).map(i => costByArea.map(x => x._2 / math.pow(x._1, i)).sum / data.length)
        params = optimizer.optimize(loss(data, _), gradLoss(data, _), initParams)
    }

    def gradLoss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Seq[Double] = {
        val trainAreas = trainSet.map(x => x._1)
        val diff = trainSet.map(x => x._3 - apply(params, (x._1, x._2)))
        def gradPj(j: Int) = -2 * diff.zip(trainAreas).map(x => x._1 * math.pow(x._2, j)).sum / diff.length
        Stream.from(0).take(params.length).map(gradPj)
    }
}

