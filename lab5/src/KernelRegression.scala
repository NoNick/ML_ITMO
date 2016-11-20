import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYPlotStyle
import org.sameersingh.scalaplot.XYSeriesImplicits.XY

class KernelRegression(K: Double => Double,
                       dist: (Double, Double) => Double = (a: Double, b: Double) => (a - b).abs) extends Regression {
    var xs: Seq[(Double, Double)] = null
    var h = 1.0

    def train(data: Seq[(Double, Double)]): Unit = {
        xs = data

        val range = Range.Double(0.1, 5.0, 0.1)
        val errors = range.map(LOO_MSE(data, _))

        val plot = XY(points = range.zip(errors), style = XYPlotStyle.Lines)
        output(PNG("plots/", "MSEByH"), xyChart(List(plot), showLegend = false))

        h = range.zip(errors).minBy(_._2)._1
    }

    def LOO_MSE(data: Seq[(Double, Double)], h: Double) = {
        this.h = h
        data.indices.map(i => {
            xs = data.zipWithIndex.filter(_._2 != i).map(_._1)
            math.pow(a(data(i)._1) - data(i)._2, 2)
        }).sum / data.size
    }

    def a(x: Double): Double = {
        val ws = xs.map(p => K(dist(x, p._1) / h))
        ws.zip(xs).map(p => p._1 * p._2._2).sum / ws.sum
    }
}
