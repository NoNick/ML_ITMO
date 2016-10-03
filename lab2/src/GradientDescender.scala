object GradientDescender extends Optimizer {
    val maxSteps = 1e2.toInt
    val alpha = 1e-15
    // poly(n) -> aplha: 1 -> 1e-2, 2 -> 1e-8, 3 -> 1e-15, 4 -> 1e-22

    override def optimize(loss: Seq[Double] => Double, grad: Seq[Double] => Seq[Double],
                          params: Seq[Double]): Seq[Double] = {
        def step(params: Seq[Double]) : Seq[Double] = {
            val result = params.zip(grad(params)).map(x => x._1 - alpha * x._2)
            result.foreach(x => print(x + ", "))
            println()
            result
        }

        Function.chain(List.fill(maxSteps)(step(_)))(params)
        //val result = step(prev)
        //println("Diff between last two descent vectors: " + result.zip(prev).map(x => math.abs(x._1 - x._2)))
        //result
    }
}
