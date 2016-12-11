import Typedefs._

import ch.ethz.dal.tinyir.lectures.TermFrequencies


// ************************ Tf-Idf model **************************

/** Provides Tf-Idf search over Tipster collection.
  * Uses inverted index for fast retrieval
  */
class TfIdfModel(docIndex : DocIndex) extends SearchEngine(docIndex) {

  /** Ranks a set of candidate documents according to TF-IDF term-model scoring criterion
    *
    * @param query       The query
    * @param candidates  Candidate documents with at least one word in common with a document in the collection
    * @return
    */
  override protected def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument] = {
    candidates.map(doc => ScoredDocument(
                      doc,query.map(term => tfIdf(index.fwIndex(doc).getOrElse(term,0).toDouble,  // Document term frequency
                                                  index.fqIndex.getOrElse(term,Map[DocId,Int]()).size.toDouble, // Per term document frequency
                                                  index.fwIndex.size.toDouble))   // Total number of documents
        .sum)).toList.sorted
  }

  /** The TF-IDF scoring function implementation
    *
    * @param termFrequency
    * @param docFrequency
    * @param numDocs
    * @return                TF-IDF score
    */
  private def tfIdf(termFrequency: Double, docFrequency : Double, numDocs : Double) : Double = {
    if (termFrequency > 0.toDouble) {
      return TermFrequencies.log2( 1.toDouble + termFrequency ) * TermFrequencies.log2(numDocs/docFrequency)
    } else {
      return 0.toDouble
    }
  }
}






// ************************ Tf-Idf test function **************************

// TODO: needs to be fixed

object TfIdfTest {
  def main(argv : Array[String]) : Int = {

//    val querys = Query.load(QUERYPATH)
//
//    val docStream = new TipsterStream(DOCUMENTPATH)
//
//    val docIndex : SimpleIndex = new SimpleIndex(new TipsterStream("./data/documents"))
//
//    val tfIdfSearch : ScoreBasedSearchEngine = new TfIdfModel(docStream, docIndex)
//
//    // languageModelSearch : ScoreBasedSearchEngine = new LanguageModel(docStream, docIndex)
//
//    // output results on queries
//    val queryString = "Some random query words "
//    println(queryString + " give results:")
//    for(scoredDoc <- tfIdfSearch.search(Set("Some","random","query", "words")).take(100)) {
//      println ("  "  + scoredDoc.id.toString + " : " + scoredDoc.score.toString)
//    }
//    // languageModelSearch.search(new Set[String]("Some","random","words")).take(100)
//
//    // TODO: Check against provided test data
    return 0
  }
}