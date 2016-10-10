class BiPolynomial2(n: Int, m: Int, initParams: Seq[Double]) extends Model{
    var params: Seq[Double] = initParams

    def poly(params: Seq[Double], v: Double): Double = params.zipWithIndex.map(p => p._1 * math.pow(v, p._2)).sum

    override def apply(params: Seq[Double], x: (Double, Double)): Double = {
        //poly(params.take(n), x._1) * poly(params.drop(n), x._2)
        (params(1) * x._1 + params(0)) * (params(3) * x._2 + params(2))
    }

    override def gradLoss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Seq[Double] = {
        val diff = trainSet.map(x => x._3 - apply(params, (x._1, x._2)))
        val grad1 = -2 * trainSet.map(x => x._1 * (x._2 * params(3) + params(2))).zip(diff).map(x => x._1 * x._2).sum / trainSet.length
        val grad0 = -2 * trainSet.map(x => x._2 * params(3) + params(2)).zip(diff).map(x => x._1 * x._2).sum / trainSet.length
        val grad3 = -2 * trainSet.map(x => x._2 * (x._1 * params(1) + params(0))).zip(diff).map(x => x._1 * x._2).sum / trainSet.length
        val grad2 = -2 * trainSet.map(x => x._1 * params(1) + params(0)).zip(diff).map(x => x._1 * x._2).sum / trainSet.length
        List(grad0, grad1, grad2, grad3)
    }

    override def learn(data: Seq[(Double, Double, Double)], optimizer: Optimizer): Unit = {
        params = optimizer.optimize(loss(data, _), gradLoss(data, _), params)
    }
}
