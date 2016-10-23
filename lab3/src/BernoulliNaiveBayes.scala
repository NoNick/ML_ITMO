class BernoulliNaiveBayes(Pr: Int => Double, p: MultinomialLikelihood) extends NaiveBayes(Pr, p) {
    def B(features: Set[Int], category: Int): Double =
        p.pPos.keys.map(f => if (features.contains(f)) p.p(f, category) else 1.0 - p.p(f, category)).sum

    override def isSpam(letter: Letter): Boolean = {
        val features = letter.features.map(_._1).toSet
        B(features, 1) > B(features, 0)
    }
}
