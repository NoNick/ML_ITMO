trait Model {
    def apply(params: Seq[Double], x: (Int, Int)): Double
    def apply(x: (Int, Int)): Double

    def learn(data: Seq[(Int, Int, Int)], optimizer: Optimizer): Unit

    def getDiffs(trainSet: Seq[(Int, Int, Int)], params: Seq[Double]): Seq[Double] = {
        val trainPoints = trainSet.map(x => (x._1, x._2))
        val trainOptima = trainSet.map(_._3)
        trainPoints.map(apply(params, _)).zip(trainOptima).map(x => x._2 - x._1)
    }

    def loss(trainSet: Seq[(Int, Int, Int)], params: Seq[Double]): Double = {
        getDiffs(trainSet, params).map(math.pow(_, 2)).sum / trainSet.length
    }

    def gradLoss(trainSet: Seq[(Int, Int, Int)], params: Seq[Double]): Seq[Double]

    def priceByArea(rooms: Int): Double => Double = x => apply((x.toInt, rooms))
}
