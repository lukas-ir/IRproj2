import Typedefs._
import ch.ethz.dal.tinyir.lectures.TermFrequencies


// ************************ Tf-Idf model **************************

/** Provides Tf-Idf search over Tipster collection.
  * Uses inverted index for fast retrieval
  */
class TfIdfModel(docIndex : DocIndex, numSearchResults : Int) extends SearchEngine(docIndex, numSearchResults) {

  /** Ranks a set of candidate documents according to TF-IDF term-model scoring criterion
    *
    * @param query       The query
    * @param candidates  Candidate documents with at least one word in common with a document in the collection
    * @return
    */
  override protected def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument] = {
    candidates.map(doc => ScoredDocument(
                      doc,query.filter(term => index.fwIndex(doc).contains(term))
                               .map(term => TfIdfModel.tfIdf(index.fwIndex(doc)(term), // Document term frequency
                                                             index.fqIndex(term).size, // Per term document frequency
                                                             index.fwIndex.size))      // Total number of documents
        .sum)).toList.sorted
  }
}


object TfIdfModel {
  /** The TF-IDF scoring function implementation
    *
    * @param termFrequency
    * @param docFrequency
    * @param numDocs
    * @return                TF-IDF score
    */
  def tfIdf(termFrequency: Int, docFrequency : Int, numDocs : Int) : Double = {
    assert(termFrequency > 0 && docFrequency < numDocs)
    TermFrequencies.log2( 1.0 + termFrequency.toDouble ) * TermFrequencies.log2(numDocs.toDouble/docFrequency.toDouble)
  }


  // ************************ Tf-Idf test function **************************

  // TODO: Test set
  def main(argv : Array[String]) = {

    val log2 = (x : Double) => math.log(x)/math.log(2.0)

    val exact_func = (tf:Int,df:Int,nd:Int) => log2( 1.0 + tf.toDouble ) * log2(nd.toDouble/df.toDouble)

    val testSet = List[(Int,Int,Int)](
      (3,2,5),
      (65,34,95),
      (33,12,1115),
      (13,24,507),
      (32,21,7923),
      (34,3,7))

    assert(testSet
      .map{ case(tf:Int,df:Int,nd:Int) => exact_func(tf,df,nd) - tfIdf(tf,df,nd) }
      .filter(math.abs(_) > 1e-14).size == 0 )
  }
}