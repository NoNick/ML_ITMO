class MultinomialLikelihood(letters: Seq[Letter]) {
    val count = new collection.mutable.HashMap[Int, (Int, Int)]
    letters.foreach(letter => letter.features.foreach(f => {
        val prev = count.getOrElse(f._1, (0, 0))
        val cur = if (letter.category == 1) (prev._1 + 1, prev._2) else (prev._1, prev._2 + 1)
        count.put(f._1, cur)
    }))
    // likelihood of being spam
    val pPos = count.map(entry => (entry._1, entry._2._1.toDouble / (entry._2._1.toDouble + entry._2._2.toDouble))).toMap
    def p(word: Int, category: Int): Double = if (category == 1) pPos.get(word).get else 1.0 - pPos.get(word).get
    def contains(word: Int): Boolean = pPos.contains(word)
}
