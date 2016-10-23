import java.io.File

/**
 * Created by noname on 10/23/16.
 */
object Main {
    val N = 10
    val data = (1 to N).map(n => loadDataset("pu1/part" + n))

    def main(args: Array[String]) : Unit = {
        println("Multinomial F1: " + crossValidate(F1, constructMultinomial))
        println("Multinomial accuracy: " + crossValidate(accuracy, constructMultinomial))
        println("Bernoulli F1: " + crossValidate(F1, constructBernoulli))
        println("Bernoulli accuracy: " + crossValidate(accuracy, constructBernoulli))
    }

    def constructMultinomial(trainSet: Seq[Letter], testSet: Seq[Letter]): NaiveBayes = {
        val Pr = new PriorProbability(trainSet)
        val p = new MultinomialLikelihood(trainSet)
        new NaiveBayes(Pr.Pr, p)
    }

    def constructBernoulli(trainSet: Seq[Letter], testSet: Seq[Letter]): NaiveBayes = {
        val Pr = new PriorProbability(trainSet)
        val p = new MultinomialLikelihood(trainSet)
        new BernoulliNaiveBayes(Pr.Pr, p)
    }

    // (true positive, false positive, true negative, false negative)
    def F1(tp: Int, fp: Int, tn: Int, fn: Int): Double = 2.0 * tp / (2.0 * tp + fn + fp)

    def accuracy(tp: Int, fp: Int, tn: Int, fn: Int): Double = (tp + tn).toDouble / (tp + fp + tn + fn)

    def crossValidate(f: (Int, Int, Int, Int) => Double, getBayes: (Seq[Letter], Seq[Letter]) => NaiveBayes): Double = {
        def evaluate(nb: NaiveBayes, testSet: Seq[Letter]): (Int, Int, Int, Int) =
            (testSet.count(l => nb.isSpam(l) && l.category == 1),
                testSet.count(l => nb.isSpam(l) && l.category == 0),
                testSet.count(l => !nb.isSpam(l) && l.category == 0),
                testSet.count(l => !nb.isSpam(l) && l.category == 1))

        (0 to (N - 1)).par.map(i => {
            val trainSet = ((if (i > 1) data.take(i - 1) else List()) ++ data.drop(i + 1)).flatten
            val testSet = data(i)
            val nb = getBayes(trainSet, testSet)

            val stats = evaluate(nb, testSet)
            f(stats._1, stats._2, stats._3, stats._4)
        }).sum / N
    }

    def loadDataset(path: String) : Seq[Letter] = {
        new File(path).listFiles.filter(_.isFile).toList.map(filename =>
            new Letter(filename.getName, scala.io.Source.fromFile(filename.getAbsolutePath).getLines().toSeq))
    }
}
