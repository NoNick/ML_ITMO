import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.Style.PointType
import org.sameersingh.scalaplot.XYPlotStyle
import org.sameersingh.scalaplot.XYSeriesImplicits.XY
import org.sameersingh.scalaplot.Style.Color._

import Main._

object Plotter {
    def showDataset(dataset: MarkedDataSet, filename: String): Unit = {
        def getByClass(classN: Int): Seq[Point] = dataset.filter(entry => entry._2 == classN).map(_._1).get

        val posPlot = XY(points = getByClass(1).map(Utils.toTuple2), style = XYPlotStyle.Points, pt = PointType.+, label = "Positives")
        val negPlot = XY(points = getByClass(0).map(Utils.toTuple2), style = XYPlotStyle.Points, pt = PointType.emptyO, label = "Negatives")
        output(PNG("plots/", filename), xyChart(List(posPlot, negPlot), pointSize = 2.5, showLegend = true))
        println("Wrote " + filename)
    }

    def showClassifiedDataset(source: MarkedDataSet, evaluated: MarkedDataSet, filename: String): Unit = {
        val parts = Utils.splitTrueFalse(source, evaluated)

        val truePos = XY(points = parts._1.map(Utils.toTuple2), style = XYPlotStyle.Points, pt = PointType.+, label = "True Positives")
        val falsePos = XY(points = parts._2.map(Utils.toTuple2), style = XYPlotStyle.Points, color = Red, pt = PointType.+, label = "False Positives")
        val trueNeg = XY(points = parts._3.map(Utils.toTuple2), style = XYPlotStyle.Points, pt = PointType.emptyO, label = "True Negatives")
        val falseNeg = XY(points = parts._4.map(Utils.toTuple2), style = XYPlotStyle.Points, color = Red, pt = PointType.emptyO, label = "False Negatives")
        output(PNG("plots/", filename), xyChart(List(truePos, falsePos, trueNeg, falseNeg), pointSize = 2.5, showLegend = true))
        println("Wrote " + filename)
    }

    def showLines(datasets: Seq[(Seq[(Double, Double)], String)], filename: String): Unit = {
        val xys = datasets.map(dataset => XY(points = dataset._1, label = dataset._2))
        output(PNG("plots/", filename), xyChart(xys, showLegend = true))
        println("Wrote " + filename)
    }
}
