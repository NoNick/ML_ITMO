import Main._

class kNN(metric : (Point, Point) => Double) {
    var k: Int = 0
    var trainSet: MarkedDataSet = Nil

    def train(data: MarkedDataSet): kNN = {
        trainSet = data
        this
    }

    def classify(points: Seq[Point]): MarkedDataSet = {
        points.map(classify)
    }

    def classify(point: Point): (Point, Int) = {
        val neighbors = trainSet.sortBy(entry => metric(point, entry._1)).take(k).partition(_._2 == 0) // binary classifier
        if (neighbors._1.size >= neighbors._2.size) (point, 0) else (point, 1)
    }

    def setParam(newK: Int) : kNN = {
        k = newK
        this
    }
}
