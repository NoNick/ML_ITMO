class PolyPerRooms(n: Int, rooms: Seq[Int]) extends Model {
    val m = rooms.length
    val polys = Stream.continually(new AreaPolynomial(n)).take(m)
    var params_ : Seq[AreaPolynomial] = Stream.continually(new AreaPolynomial(n)).take(m)
    def params() = params_.flatMap(_.params)

    def shape(unshaped: Seq[Double]): Seq[Seq[Double]] =
        Stream.from(0).take(m).map(i => unshaped.slice(i * m, (i + 1) * m))
    def unshape(shaped: Seq[Seq[Double]]): Seq[Double] = shaped.flatMap(x => x)

    def getIndex(roomN: Double): Int =
        rooms.zipWithIndex.map(x => (math.abs(roomN - x._1), x._2)).min._2

    override def apply(ps: Seq[Double], x: (Double, Double)): Double = {
        params_(getIndex(x._2)).apply(ps, x)
    }

    override def learn(data: Seq[(Double, Double, Double)], optimizer: Optimizer): Unit = {
        rooms.map(r => data.filter(r == _._2)).zip(params_).foreach(x => x._2.learn(x._1, optimizer))
    }

    override def gradLoss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Seq[Double] = ???
}
