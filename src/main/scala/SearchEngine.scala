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

  /** Perform search for multiple queries on document collection
    *
    * @param queries      Query ID to query string map
    * @return             Query ID to ranked search results map
    */
  def fastSearch(queries : Map[QueryId,String]) : Map[QueryId,List[ScoredDocument]] = {
    queries.mapValues(fastSearch(_))
  }

  /** Perform search on document collection
    *
    * @param query        Search query string
    * @return             Ranked search results
    */
  def fastSearch(query : String) : List[ScoredDocument] = {
    println("Fast search on collection for query : "+query)
    val tokenizedQuery : Set[Term] = Tokenizer.tokenize(query).toSet
    rank(tokenizedQuery,tokenizedQuery.flatMap(
      term => index.fqIndex.getOrElse(term,Map[DocId,Int]()).keySet)).take(numResults)
  }


  /** Perform search for multiple queries on document collection
    *
    * @param queries      Query ID to query string map
    * @return             Query ID to ranked search results map
    */
  def slowSearch(queries : Map[QueryId,String]) : Map[QueryId,List[ScoredDocument]] = {
    queries.mapValues(slowSearch(_))
  }

  /** Perform slow search on document collection without inverted index
    *
    * @param query        Search query string
    * @return             Ranked search results
    */
  def slowSearch(query : String) : List[ScoredDocument] = {
    println("Slow search on collection for query : "+query)
    val tokenizedQuery : Set[Term] = Tokenizer.tokenize(query).toSet
    rank(tokenizedQuery,index.fwIndex.filter( docTermFreq => docTermFreq._2.keySet.exists(tokenizedQuery.contains) ).keySet).take(numResults)
  }



  /** Rank a set of candidate documents on tokenized query
    *
    * @param query       tokenized query
    * @param candidates  candidate documents
    * @return            Ranked list of documents with scores
    */
  private def rank(query : Set[Term], candidates : Set[DocId]) : List[ScoredDocument] = {
    if (candidates.size < 100)
      println("Performing ranking on less than 100 models for query : "+query)
    candidates.map(doc => ScoredDocument( doc.intern(),score(query,doc) ) ).toList.sorted
  }

  /** Rank a set of candidate documents on tokenized query
    *
    * @param query       tokenized query
    * @param candidate  candidate document
    * @return            Ranked list of documents with scores
    */
  def score(query : Set[Term], candidate : DocId) : Double
}

