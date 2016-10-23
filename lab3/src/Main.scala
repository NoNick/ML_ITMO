import java.io.File

/**
 * Created by noname on 10/23/16.
 */
object Main {
    val data = Stream.from(1).take(10).map(n => loadDataset("/home/noname/ML/lab3/pu1/part" + n))
    val Pr = new PriorProbability(data.take(9).flatten)
    val p = new MultinomialLikelihood(data.take(9).flatten)
    val nb = new NaiveBayesian(Pr.Pr, p)

    def main(args: Seq[String]) : Unit = {

    }

    def loadDataset(path: String) : Seq[Letter] = {
        new File(path).listFiles.filter(_.isFile).toList.map(filename =>
            new Letter(filename.getName, scala.io.Source.fromFile(filename.getAbsolutePath).getLines().toSeq))
    }
}
