import math.log
import collection.mutable

class LanguageModel(index: DocIndex) {
  private def predictOneDoc(doc: String, querytoken: Seq[String]) = {
    val pwd = index.fwIndex(doc)
      .toMap
      .mapValues(_.toDouble / index.ntokensdoc(doc))
    val pw = index.fqIndex.mapValues(_.map(_._2).sum.toDouble / index.ntokens)
    var likelihood = 0.0
    for (word <- querytoken.intersect(pwd.keySet.toList)) {
      likelihood = log(1+(1-index.lambdad(doc))/index.lambdad(doc)*pwd(word)/pw(word)) + log(index.lambdad(doc))
    }
    likelihood
  }

  def predict(query: String): List[(String, Double)] = {
    var count = 0
    val querytoken = Tokenizer.tokenize(query)
    val likelihood = index.fwIndex.map{
      case (doc,_) => (doc.intern(), predictOneDoc(doc, querytoken))
    }

    likelihood.filter(_._2 != 0.0).toList.sortBy(- _._2).take(100)
  }
}
