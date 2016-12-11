import Typedefs._

// ************************ Search engine **************************

/** Base class for fast two-step search engine retrieving all
  * candidate results first from an inverted index
  * and evaluating score on them subsequently
  */
abstract class SearchEngine(docIndex : DocIndex) {
  protected val index : DocIndex = docIndex

  // Scored Document, on purpose permuted compare member to have descending sort order
  case class ScoredDocument(val doc: DocId, val score : Double) extends Ordered[ScoredDocument] {
    def compare(that: ScoredDocument) = that.score compare this.score
  }

  /** Perform search on document collection
    *
    * @param query        Search query string
    * @param numResults   Number of requested results
    * @return             Ranked search results
    */
  def search(query : String, numResults : Int) : List[ScoredDocument] = {
    println("Searching collection for query : "+query)
    val tokenizedQuery : Set[Term] = Tokenizer.tokenize(query).toSet
    rank(tokenizedQuery,tokenizedQuery.flatMap(
      term => index.fqIndex.getOrElse(term,Map[DocId,Int]()).keySet)).take(numResults)
  }

  /** Perform search for multiple queries on document collection
    *
    * @param queries      Query ID to query string map
    * @param numResults   Number of requested results
    * @return             Query ID to ranked search results map
    */
  def search(queries : Map[Int,String], numResults : Int) : Map[Int,List[ScoredDocument]] = {
    queries.mapValues(search(_,numResults))
  }


  protected def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument]
}

