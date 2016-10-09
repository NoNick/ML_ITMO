class PolyPerRooms(n: Int, rooms: Seq[Int]) extends Model {
    val m = rooms.length
    val polys = Stream.continually(new AreaPolynomial(n)).take(m)
    var params: Seq[AreaPolynomial] = Stream.continually(new AreaPolynomial(n)).take(m)

    def shape(unshaped: Seq[Double]): Seq[Seq[Double]] =
        Stream.from(0).take(m).map(i => unshaped.slice(i * m, (i + 1) * m))
    def unshape(shaped: Seq[Seq[Double]]): Seq[Double] = shaped.flatMap(x => x)

    def getIndex(roomN: Int): Int =
        rooms.zipWithIndex.map(x => (math.abs(roomN - x._1), x._2)).min._2

    override def apply(ps: Seq[Double], x: (Int, Int)): Double = {
        params(getIndex(x._2)).apply(ps, x)
    }

    override def apply(x: (Int, Int)): Double = apply(params(getIndex(x._2)).params, x)

    override def learn(data: Seq[(Int, Int, Int)], optimizer: Optimizer): Unit = {
        rooms.map(r => data.filter(r == _._2)).zip(params).foreach(x => x._2.learn(x._1, optimizer))
    }

    override def gradLoss(trainSet: Seq[(Int, Int, Int)], params: Seq[Double]): Seq[Double] = ???
}
