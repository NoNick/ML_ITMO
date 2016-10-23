
class Letter(filename: String, content: Seq[String]) {
    val category = if (filename.contains("spmsg")) 1 else 0
    val titleFeatures = countFeatures(content(0).replaceAll("Subject: ", ""))
    val bodyFeatures = countFeatures(content(2))
//    val features = countFeatures(content(0).replaceAll("Subject: ", "") + " " + content(2))
    val features = (titleFeatures.toSeq ++ bodyFeatures.toSeq.map(f => (-f._1, f._2))).toMap

    def countFeatures(input: String): Map[Int, Int] =
        input.split(" ").filter(_.size != 0).map(_.toInt).groupBy(identity).mapValues(_.size)
}