trait Model {
    var avgArea = Double.NaN
    var avgRooms = Double.NaN
    def normalize(data: Seq[(Double, Double, Double)]) = {
        if (avgArea.isNaN) avgArea = data.map(_._1).sum / data.length
        if (avgRooms.isNaN) avgRooms = data.map(_._2).sum / data.length
        data.map(x => (x._1 / avgArea, x._2 / avgRooms, x._3))
    }

    def params(): Seq[Double]

    def apply(params: Seq[Double], x: (Double, Double)): Double
    def apply(x: (Double, Double)): Double = apply(params(), x)

    def learn(data: Seq[(Double, Double, Double)], optimizer: Optimizer): Unit

    def getDiffs(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Seq[Double] = {
        val trainPoints = trainSet.map(x => (x._1, x._2))
        val trainOptima = trainSet.map(_._3)
        trainPoints.map(apply(params, _)).zip(trainOptima).map(x => x._2 - x._1)
    }

    def loss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Double =
        getDiffs(trainSet, params).map(math.pow(_, 2)).sum / trainSet.length

    def linearLoss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Double = {
        val diffs = getDiffs(trainSet, params).map(math.abs)
        diffs.sum / diffs.length
    }

    def gradLoss(trainSet: Seq[(Double, Double, Double)], params: Seq[Double]): Seq[Double]

    def priceByAreaNonnorm(rooms: Double): Double => Double = x => apply((x / avgArea, rooms / avgRooms))
}
