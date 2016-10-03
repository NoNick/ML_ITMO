trait Optimizer {
    def optimize(loss: Seq[Double] => Double,
                 grad: Seq[Double] => Seq[Double],
                 params: Seq[Double]) : Seq[Double]
}
