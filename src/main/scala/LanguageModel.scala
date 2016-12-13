import Typedefs._
import math.log


/** Bag-of-words multinomial maximum likelihood language model
  *
  * @param docIndex  Inverted index for fast retrieval
  *
  * TODO: Cleanup of cross-dependencies on DocIndex class (move all here)
  */
class NewLanguageModel(docIndex: DocIndex, numSearchResults : Int) extends SearchEngine(docIndex, numSearchResults) {

  // Witten Bell Smoothing
  val lambdad = docIndex.fwIndex.mapValues{ tfmap =>
    val cf = tfmap.values.sum.toDouble
    cf / (cf + tfmap.size)
  }

  val ntokensdoc = docIndex.fwIndex.mapValues(_.values.sum)

  val ntokens = ntokensdoc.foldLeft(0)(_ + _._2)

  val pw = docIndex.fqIndex
                        .mapValues(_.values.sum.toDouble / ntokens)

  override def score(query : Set[Term], doc : DocId) : Double = {
    val lmbd = lambdad(doc)
    query.intersect(index.fwIndex(doc).keySet)                    /* get query tokens occuring in document */
                 .map { word =>
                   val pwd = index.fwIndex(doc)(word).toDouble / ntokensdoc(doc) /* query token frequency divided by total number of tokens in document */
                   val pw = pw(word)                              /* relative collection frequency of query token */
                   log(1 + (1 - lmbd) / lmbd * pwd / pw)          /* query-token dependent contribution to document score */
                 }.sum + log(lmbd)                                /* document-dependent Jelinek-Mercer smoothing parameter */
  }

}