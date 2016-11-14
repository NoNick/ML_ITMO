import Main._

trait Classifier {
    def train(data: MarkedDataSet): Unit // changes state

    def classify(points: Seq[Point]): MarkedDataSet
}
