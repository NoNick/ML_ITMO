class PriorProbability(letters: Seq[Letter]) {
    val byCategory = letters.groupBy(_.category).mapValues(_.size)
    def Pr(c: Int): Double = byCategory.getOrElse(c, 0).toDouble / letters.size.toDouble
}
