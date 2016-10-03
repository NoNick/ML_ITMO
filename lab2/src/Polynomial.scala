class Polynomial(n: Int) extends Model {
    val maxParam = 1e5
    def initParamsPoly(n: Int): Seq[Double] = Stream.from(1).take(n).map(i => math.pow(maxParam, 1.0 / i))

    override def defaultParams: Seq[Double] = initParamsPoly(n) ++ initParamsPoly(n) ++ initParamsPoly(n)

    override def apply(params: Seq[Double], x: (Int, Int)): Double = {
        val offset = n * (if (x._2 <= 3) {
                              if (x._2 == 3)  1
                              else            0
                          } else              2)
        Stream.from(0).take(n).map(i => params(i + offset) * math.pow(x._1, i)).sum
        //Stream.from(0).take(n).map(i => params(i) * math.pow(x._1, i)).sum
    }

}
