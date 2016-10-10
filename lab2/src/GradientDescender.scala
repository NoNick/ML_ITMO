class GradientDescender(alpha: Double, maxSteps: Int) extends Optimizer {
    // poly(n) -> alpha: 1 -> 1e-2, 2 -> 1e-8, 3 -> 1e-15, 4 -> 1e-22

    override def optimize(loss: Seq[Double] => Double, grad: Seq[Double] => Seq[Double],
                          params: Seq[Double]): Seq[Double] = {
        def step(params: Seq[Double]) : Seq[Double] = {
            val g = grad(params)
            val result = params.zip(g).map(x => x._1 - alpha * x._2)
            result.foreach(x => print(x + ", "))
            print("\t" + loss(params))
            println()
            result
        }

        Function.chain(List.fill(maxSteps)(step(_)))(params)
        //val result = step(prev)
        //println("Diff between last two descent vectors: " + result.zip(prev).map(x => math.abs(x._1 - x._2)))
        //result
    }
}
