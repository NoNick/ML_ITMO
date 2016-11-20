import scala.io.Source
import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYPlotStyle
import org.sameersingh.scalaplot.XYSeriesImplicits.XY

object Main {
    def Gauss(r: Double): Double = math.exp(-r * r / 2)

    def Square(r: Double): Double = if (r < 1) math.pow(1 - r * r, 2) else 0

    def main(args: Array[String]): Unit = {
        val data = load("non-parametric.csv")
        showDataset(data, "source")

        val gaussRegression = new KernelRegression(Gauss)
        gaussRegression.train(data)
        showRegression(data, gaussRegression, "gauss")
        System.out.println("MSE for nonparametric with Gauss kernel is " + gaussRegression.MSE(data))

        val lowess = new LOWESS(Gauss, Square)
        lowess.train(data)
        showRegression(data, lowess, "lowess")
        System.out.println("MSE for nonparametric with Gauss kernel is " + lowess.MSE(data))
    }

    def showDataset(dataset: Seq[(Double, Double)], filename: String): Unit = {
        val plot = XY(points = dataset, style = XYPlotStyle.Points)
        output(PNG("plots/", filename), xyChart(List(plot), showLegend = false))
        println("Wrote " + filename)
    }

    def showRegression(dataset: Seq[(Double, Double)], r: Regression, filename: String): Unit = {
        val source = XY(points = dataset, style = XYPlotStyle.Points)
        val regression =  XY(points = r.points(Range.Double(0, 60, 1)), style = XYPlotStyle.Lines)
        output(PNG("plots/", filename), xyChart(List(source, regression), showLegend = false))
        println("Wrote " + filename)
    }

    /**
     * Loads dataset from file.
     *
     * @param path input file
     * @return (x, y) samples
     */
    def load(path: String): Seq[(Double, Double)] =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(";")
            (words(1).toDouble, words(2).toDouble)
        }.toSeq

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }
}
