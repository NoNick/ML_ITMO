/**
 * Created by noname on 10/23/16.
 */
class NaiveBayes(Pr: Int => Double, p: MultinomialLikelihood) {
    def B(features: Seq[(Int, Int)], category: Int): Double = math.log(Pr(category)) +
         features.map(f => f._2 * math.log(p.p(f._1, category))).sum

    def isSpam(letter: Letter): Boolean = {
        val filtered = letter.features.filter(f => p.contains(f._1)).toSeq
        B(filtered, 1) > B(filtered, 0)
    }
}
