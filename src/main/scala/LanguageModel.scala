import Typedefs._

import math.log

// TODO: Decide for a single implementation

/*
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
*/

/** Bag-of-words multinomial maximum likelihood language model
  *
  * @param docIndex  Inverted index for fast retrieval
  *
  * TODO: Cleanup of cross-dependencies on DocIndex class (move all here)
  */
class NewLanguageModel(docIndex: DocIndex, numSearchResults : Int) extends SearchEngine(docIndex, numSearchResults) {

  override def score(query : Set[Term], doc : DocId) : Double = {
    val lmbd = index.lambdad(doc)
    query.intersect(index.fwIndex(doc).keySet)                    /* get query tokens occuring in document */
                 .map { word =>
                   val pwd = index.fwIndex(doc)(word).toDouble / index.ntokensdoc(doc) /* query token frequency divided by total number of tokens in document */
                   val pw = index.pw(word)                        /* relative collection frequency of query token */
                   log(1 + (1 - lmbd) / lmbd * pwd / pw)          /* query-token dependent contribution to document score */
                 }.sum + log(lmbd)                                /* document-dependent Jelinek-Mercer smoothing parameter */
  }

}