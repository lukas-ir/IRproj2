import Typedefs._

import math.log

/** Bag-of-words multinomial maximum likelihood language model
  *
  * @param index  Inverted index for fast retrieval
  *
  * TODO: Cleanup of cross-dependencies on DocIndex class
  */
class LanguageModel(index: DocIndex) {

  private def predictOneDoc(doc: String, qrtk: Seq[String]) = {
    val lmbd = index.lambdad(doc)
    qrtk.toSet
      .intersect(index.fwIndex(doc).keySet)
      .map { word =>
        val pwd = index.fwIndex(doc)(word).toDouble / index.ntokensdoc(doc)
        val pw = index.pw(word)
        log(1 + (1 - lmbd) / lmbd * pwd / pw)
      }
      .sum + log(lmbd)
  }

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

// TODO: move all the index references to this class
class NewLanguageModel(docIndex: DocIndex) extends SearchEngine(docIndex) {

  override protected def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument] = {
    candidates.map(doc => (doc,index.lambdad(doc)))
              .map{
      case (doc,lmbd) => ScoredDocument(
        doc.intern(),query.intersect(index.fwIndex(doc).keySet)
                 .map { word =>
                   val pwd = index.fwIndex(doc)(word).toDouble / index.ntokensdoc(doc)
                   val pw = index.pw(word)
                   log(1 + (1 - lmbd) / lmbd * pwd / pw)
                 }.sum + log(lmbd)
        )}
      .toList.sorted
  }

}