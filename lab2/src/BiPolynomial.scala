class BiPolynomial(n: Int, m: Int) extends Model{
    var params: Seq[Double] = List.fill(n)(1.0) ++ List.fill(m)(1.0)

    def poly(params: Seq[Double], v: Double): Double = params.zipWithIndex.map(p => p._1 * math.pow(v, p._2)).sum

    override def apply(params: Seq[Double], x: (Int, Int)): Double = {
        poly(params.take(n), x._1) * poly(params.drop(n), x._2)
    }

    override def gradLoss(trainSet: Seq[(Int, Int, Int)], params: Seq[Double]): Seq[Double] = {
        val diff = getDiffs(trainSet, params)
        val fst = trainSet.map(_._1)
        val snd = trainSet.map(_._2)
        def gradJ(j: Int, a: Seq[Int], b: Seq[Int], p: Seq[Double]): Double = a.map(math.pow(_, j)).zip(b).
            map(x => x._1 * poly(p, x._2)).zip(diff).map(x => x._1 * x._2).sum * (-2.0 / diff.length)
        Stream.from(0).take(n).map(gradJ(_, fst, snd, params.drop(n))) ++
            Stream.from(0).take(m).map(gradJ(_, snd, fst, params.take(n)))
    }

    override def apply(x: (Int, Int)): Double = apply(params, x)

    override def learn(data: Seq[(Int, Int, Int)], optimizer: Optimizer): Unit = {
        params = optimizer.optimize(loss(data, _), gradLoss(data, _), params)
    }
}
