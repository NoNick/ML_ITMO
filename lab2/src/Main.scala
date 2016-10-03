import javafx.scene.chart.XYChart

import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYSeriesImplicits.XY

import scala.io.Source

object Main {
    val model = new Polynomial(3)
    val optimizer = GradientDescender

    def main(args: Array[String]): Unit = {
        //val dataset = load("prices.txt").filter(x => x._2 >= 2 && x._2 <= 4).sortBy(_._1)
        val dataset = load("prices.txt").filter(x => x._2 == 3).sortBy(_._1)

        model.learn(dataset, optimizer)
        val possibleRooms = dataset.map(_._2).distinct
        val minArea = dataset.map(_._1).min
        val maxArea = dataset.map(_._1).max
        val xys = possibleRooms.map(rooms => XY(points = priceByArea(dataset, rooms), label = rooms + " rooms, given")) ++
                  possibleRooms.map(rooms => XY(evalXY(model.priceByArea(rooms), minArea, maxArea, 20), rooms + " rooms, trained"))

        output(PNG("plots/", "room1"), xyChart(xys, showLegend = true, size = (1000.0, 800.0)))
        //println(params.foreach(x => print(x + ", ")))
    }

    def priceByArea(dataset: Seq[(Int, Int, Int)], rooms: Int): Seq[(Double, Double)] =
        dataset.filter(_._2 == rooms).map(x => (x._1.toDouble, x._3.toDouble)).toSeq

    def evalXY(f: Double => Double, left: Double, right: Double, pointN: Int): Seq[(Double, Double)] = {
        val diff = (right - left) / pointN
        Stream.from(0).take(pointN).map(i => left + i * diff).map(x => (x, f(x)))
    }

    def load(path: String): Seq[(Int, Int, Int)] =
        Source.fromFile(path).getLines().map { line =>
            val words = line.split(",")
            (words(0).toInt, words(1).toInt, words(2).toInt)
        }.toSeq
}
