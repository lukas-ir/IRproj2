import Typedefs._
import math.log


/** Bag-of-words multinomial maximum likelihood language model
  *
  * @param docIndex  Inverted index for fast retrieval
  */
class LanguageModel(docIndex: DocIndex, numSearchResults : Int) extends SearchEngine(docIndex, numSearchResults) {

  override def score(query : Set[Term], doc : DocId) : Double = {
    // Witten Bell Smoothing
    val lmbd = index.lmLambdaD(doc)
    query.intersect(index.fwIndex(doc).keySet)                    /* get query tokens occuring in document */
                 .map { word =>
                   val pwd = index.fwIndex(doc)(word).toDouble / index.fwIndex(doc).values.sum.toDouble /* query token frequency divided by total number of tokens in document */
                   val pw =  index.cfInvIndex(word).toDouble   / index.cumNumTokens.toDouble  /* relative collection frequency of query token */
                   log(1 + (1 - lmbd) / lmbd * pwd / pw)          /* query-token dependent contribution to document score */
                 }.sum + log(lmbd)                                /* document-dependent Jelinek-Mercer smoothing parameter */
  }
}