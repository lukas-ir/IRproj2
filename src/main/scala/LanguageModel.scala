import math.log


class LanguageModel(index: DocIndex) {


  def sIntersect(l1: List[String], l2: List[String]) : List[String] = {
    def iter (l1: List[String], l2: List[String], result: List[String]) : List[String] = {
      if (l1.isEmpty || l2.isEmpty)
        result.reverse
      else if (l1.head > l2.head) iter(l1, l2.tail,result)  // advance list l2
      else if (l1.head < l2.head) iter(l1.tail, l2,result)  // advance list l1
      else                        iter(l1.tail, l2.tail, l1.head::result)
    }

    iter(l1,l2,Nil)
  }


  private def predictOneDoc(doc: String, querytoken: Seq[String]) = {
    var likelihood = 0.0
    val common = querytoken.toSet.intersect(index.fwIndex(doc).keySet)
    println(common)
    for (word <- common) {
//    for (word <- sIntersect(querytoken.toList.sorted, index.fwIndex.keySet.toList.sorted)) {
      val pwd = index.fwIndex(doc)(word).toDouble / index.ntokensdoc(doc)
      likelihood += log(1+(1-index.lambdad(doc))/index.lambdad(doc)*pwd/index.pw(word)) + log(index.lambdad(doc))
    }
    likelihood
  }

  def predict(query: String): List[(String, Double)] = {
    println("predicting "+query)
    var count = 0
    val querytoken = Tokenizer.tokenize(query)
    println(querytoken)
    val likelihood = index.fwIndex.map{
      case (doc,_) => {
        (doc.intern(), predictOneDoc(doc, querytoken))
      }
    }

    for (i <- likelihood.filter(_._2 != 0.0).toList.sortBy(- _._2).take(100)){
      println(i)
    }
    println("finished")

    likelihood.filter(_._2 != 0.0).toList.sortBy(- _._2).take(100)
  }
}
