import Typedefs._

// ************************ Search engine **************************

// Scored Document, on purpose permuted compare member to have descending sort order
case class ScoredDocument(val doc: DocId, val score : Double) extends Ordered[ScoredDocument] {
  def compare(that: ScoredDocument) = that.score compare this.score
}

/** Base class for fast two-step search engine retrieving all
  * candidate results first from an inverted index
  * and evaluating score on them subsequently
  */
abstract class SearchEngine(docIndex : DocIndex, numSearchResults : Int) {
  // inverted index and
  protected val index : DocIndex = docIndex

  protected val numResults = numSearchResults

  /** Perform search on document collection
    *
    * @param query        Search query string
    * @return             Ranked search results
    */
  def search(query : String) : List[ScoredDocument] = {
    println("Searching collection for query : "+query)
    val tokenizedQuery : Set[Term] = Tokenizer.tokenize(query).toSet
    rank(tokenizedQuery,tokenizedQuery.flatMap(
      term => index.fqIndex.getOrElse(term,Map[DocId,Int]()).keySet)).take(numResults)
  }

  /** Perform search for multiple queries on document collection
    *
    * @param queries      Query ID to query string map
    * @return             Query ID to ranked search results map
    */
  def search(queries : Map[QueryId,String]) : Map[QueryId,List[ScoredDocument]] = {
    queries.mapValues(search(_))
  }

  /** Rank a set of candidate documents on tokenized query
    *
    * @param query       tokenized query
    * @param candidates  candidate documents
    * @return            Ranked list of documents with scores
    */
  protected def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument]
}

