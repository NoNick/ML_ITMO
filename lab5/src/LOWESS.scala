import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYPlotStyle
import org.sameersingh.scalaplot.XYSeriesImplicits.XY

class LOWESS(K1: Double => Double, K2: Double => Double,
             dist: (Double, Double) => Double = (a: Double, b: Double) => (a - b).abs) extends Regression {
    var xs: Seq[(Double, Double)] = null
    var gamma: Seq[Double] = null
    var h = 1.1

    val maxIterations = 900
    val eps = 1e-2

    def train(data: Seq[(Double, Double)]): Unit = {
        val range = Range.Double(0.1, 5.0, 0.1)
        val errors = range.map(LOO_MSE(data, _))

        val plot = XY(points = range.zip(errors), style = XYPlotStyle.Lines)
        output(PNG("plots/", "MSEByH.LOWESS"), xyChart(List(plot), showLegend = false))

        h = range.zip(errors).minBy(_._2)._1

        gamma = List.fill(data.size)(1.0)
        trainGamma(data)
        xs = data
    }

    def LOO_MSE(data: Seq[(Double, Double)], h: Double) = {
        this.h = h
        gamma = List.fill(data.size)(1.0)
        trainGamma(data)
        val gamma_ = gamma
        data.indices.map(i => {
            xs = data.zipWithIndex.filter(_._2 != i).map(_._1)
            gamma = gamma_.zipWithIndex.filter(_._2 != i).map(_._1)
            math.pow(a(data(i)._1) - data(i)._2, 2)
        }).sum / data.size
    }

    def trainGamma(data: Seq[(Double, Double)], iteration: Int = 0): Unit = {
        val error = data.indices.map(i => {
            xs = data.zipWithIndex.filter(_._2 != i).map(_._1)
            (a(data(i)._1) - data(i)._2).abs
        })
        val eMedian = error.sum / error.size
        val gamma_ = error.map(e => K2(e / (6 * eMedian)))

        val stable = gamma.zip(gamma_).map(g => (g._1 - g._2).abs).max <= eps
        gamma = gamma_
        if (!stable && iteration < maxIterations) {
            trainGamma(data, iteration + 1)
        }
    }

    def a(x: Double): Double = {
        val ws = xs.zip(gamma).map(p => p._2 * K1(dist(x, p._1._1) / h))
        ws.zip(xs).map(p => p._1 * p._2._2).sum / ws.sum
    }
}
