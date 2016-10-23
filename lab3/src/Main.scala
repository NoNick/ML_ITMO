import java.io.File

/**
 * Created by noname on 10/23/16.
 */
object Main {
    val N = 10
    val data = Stream.from(1).take(N).map(n => loadDataset("pu1/part" + n))

    def main(args: Array[String]) : Unit = {
        println("F1: " + crossValidate(F1))
        println("accuracy: " + crossValidate(accuracy))
    }

    // (true positive, false positive, true negative, false negative)
    def F1(tp: Int, fp: Int, tn: Int, fn: Int): Double = 2.0 * tp / (2.0 * tp + fn + fp)

    def accuracy(tp: Int, fp: Int, tn: Int, fn: Int): Double = (tp + tn).toDouble / (tp + fp + tn + fn)

    def crossValidate(f: (Int, Int, Int, Int) => Double): Double = {
        def evaluate(nb: NaiveBayesian, testSet: Seq[Letter]): (Int, Int, Int, Int) =
            (testSet.count(l => nb.isSpam(l) && l.category == 1),
                testSet.count(l => nb.isSpam(l) && l.category == 0),
                testSet.count(l => !nb.isSpam(l) && l.category == 0),
                testSet.count(l => !nb.isSpam(l) && l.category == 1))

        Stream.from(0).take(N).map(i => {
            val trainSet = ((if (i > 1) data.take(i - 1) else List()) ++ data.drop(i + 1)).flatten
            val testSet = data(i)
            val Pr = new PriorProbability(trainSet)
            val p = new MultinomialLikelihood(trainSet)
            val nb = new NaiveBayesian(Pr.Pr, p)

            val stats = evaluate(nb, testSet)
            f(stats._1, stats._2, stats._3, stats._4)
        }).sum / N
    }

    def loadDataset(path: String) : Seq[Letter] = {
        new File(path).listFiles.filter(_.isFile).toList.map(filename =>
            new Letter(filename.getName, scala.io.Source.fromFile(filename.getAbsolutePath).getLines().toSeq))
    }
}
