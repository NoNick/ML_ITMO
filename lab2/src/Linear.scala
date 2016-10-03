object Linear extends Model {
    override def defaultParams: Seq[Double] = List(100, 1000000, 100000)

    override def apply(params: Seq[Double], x: (Int, Int)): Double = params(0) * x._1 + params(1) * x._2 + params(2)
}
