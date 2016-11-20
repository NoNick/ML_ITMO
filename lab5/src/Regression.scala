trait Regression {
    def train(data: Seq[(Double, Double)]): Unit
    def a(x: Double): Double
    def points(range: Seq[Double]): Seq[(Double, Double)] = range.par.map(r => (r, a(r))).seq
    def MSE(test: Seq[(Double, Double)]) = test.map(t => math.pow(a(t._1) - t._2, 2)).sum / test.size
}
