import org.sameersingh.scalaplot.Style.PointType
import org.sameersingh.scalaplot.XYPlotStyle

import scala.io.Source
import org.sameersingh.scalaplot.Implicits._

object Main {
    type Point = (Double, Double)
    type MarkedDataSet = Seq[(Point, Int)]  // (point, # of class)

    def main(args: Array[String]): Unit = {
        val data = load("chips.txt")
        show(data, "source")
        showSampleClassification(data, 5)
    }

    def showSampleClassification(dataset: MarkedDataSet, k: Int): Unit = {
        val splitted = SingleParameterFitter.splitForValidation(dataset, 5)(0)
        val testSet = splitted._2.map(entry => entry._1)
        // TODO: curry
        val classified = new kNN(k, p => euclidMetric(p, _), splitted._1).classify(testSet)
        show(classified, "classified")
    }

    def euclidMetric(p1: Point, p2: Point) : Double = math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

    def show(dataset: MarkedDataSet, filename: String): Unit = {
        def getByClass(classN: Int): Seq[Point] = dataset.filter(entry => entry._2 == classN).map(_._1).get

        val posPlot = XY(points = getByClass(1), style = XYPlotStyle.Points, pt = PointType.+, label = "Positives")
        val negPlot = XY(points = getByClass(0), style = XYPlotStyle.Points, pt = PointType.emptyO, label = "Negatives")
        output(PNG("./", filename), xyChart(List(posPlot, negPlot), pointSize = 2.5))
    }

    /**
     * Loads dataset from file.
     *
     * @param path input file
     * @return (positive, negative) samples
     */
    def load(path: String): MarkedDataSet =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(",")
            ((words(0).toDouble, words(1).toDouble), words(2).toInt)
        }.get.toSeq

    def toPair(a: Iterator[Array[Double]]): Iterator[Point] = a.map(a => (a(0), a(1)))
}
