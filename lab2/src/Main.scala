import javafx.scene.chart.XYChart

import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYSeriesImplicits.XY

import scala.io.Source

object Main {
    val model = new BiPolynomial2(2, 2, List(1.0, 1.0, 1.0, 1.0))// (ax + b) * (cy + d)
    //val optimizer = new GradientDescender(1e-7, 1e3.toInt)
    //val model = new PolyPerRooms(3, List(2,3,4))
    //val optimizer = new GradientDescender(1e-15, 1e3.toInt)
    val optimizer = GeneticOptimizer

    def main(args: Array[String]): Unit = {
        var dataset = load("prices.txt").sortBy(_._1)
        dataset = dataset.filter(x => x._2 >= 2 && x._2 <= 4)
        val normData = model.normalize(dataset)

        model.learn(normData, optimizer)
        println("Linear loss: " + model.linearLoss(normData, model.params))
        val possibleRooms = dataset.map(_._2.toInt).distinct
        val minArea = dataset.map(_._1).min
        val maxArea = dataset.map(_._1).max
        val xys = possibleRooms.map(rooms => XY(points = priceByArea(dataset, rooms), label = rooms + " rooms, given")) ++
                  possibleRooms.map(rooms => XY(evalXY(model.priceByAreaNonnorm(rooms / model.avgRooms), minArea, maxArea, 20),
                      rooms + " rooms, trained"))

        output(PNG("plots/", "room1"), xyChart(xys, showLegend = true, size = (1000.0, 800.0)))
        //println(params.foreach(x => print(x + ", ")))
    }

    def getModel(): Model = {
        val dataset = load("prices.txt").sortBy(_._1)
        model.learn(dataset, optimizer)
        model
    }

    def priceByArea(dataset: Seq[(Double, Double, Double)], rooms: Double): Seq[(Double, Double)] =
        dataset.filter(x => math.abs(x._2 - rooms) < 1e-6).map(x => (x._1, x._3)).toSeq

    def evalXY(f: Double => Double, left: Double, right: Double, pointN: Int): Seq[(Double, Double)] = {
        val diff = (right - left) / pointN
        Stream.from(0).take(pointN).map(i => left + i * diff).map(x => (x, f(x)))
    }

    def load(path: String): Seq[(Double, Double, Double)] =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(",")
            (words(0).toDouble, words(1).toDouble, words(2).toDouble)
        }.toSeq
}
