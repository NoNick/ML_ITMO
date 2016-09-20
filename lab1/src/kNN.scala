import Main._

class kNN(param: Int, metric : Point => Point => Double, train: MarkedDataSet) {
    var k = param

    def classify(points: Seq[Point]): MarkedDataSet = {
        points.map(classify)
    }

    def classify(point: Point): (Point, Int) = {
        val neighbors = train.sortBy(entry => metric(point)(entry._1)).take(k).partition(_._2 == 0) // binary classifier
        if (neighbors._1.size >= neighbors._2.size) (point, 0) else (point, 1)
    }

    def setK(newK: Int) : Unit = k = newK
}
