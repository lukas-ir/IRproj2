import math.log


class LanguageModel(index: DocIndex) {

  private def predictOneDoc(doc: String, qrtk: Seq[String]) =
    qrtk.toSet
        .intersect(index.fwIndex(doc).keySet)
        .map{ word =>
          val pwd = index.fwIndex(doc)(word).toDouble / index.ntokensdoc(doc)
          val lmbd = index.lambdad(doc)
          val pw = index.pw(word)
          log(1+(1-lmbd)/lmbd*pwd/pw) + log(lmbd)
        }
        .sum

  def predict(query: String): List[(String, Double)] = {
    println("predicting "+query)
    val querytoken = Tokenizer.tokenize(query)
    println(querytoken)

    index.fwIndex
         .keySet
         .map(doc => (doc.intern(), predictOneDoc(doc, querytoken)))
         .filter(_._2 != 0.0)
         .toList
         .sortBy(- _._2)
         .take(100)

  }
}
