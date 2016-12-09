import java.util
import java.util.stream.Stream
import javax.print.Doc

import ch.ethz.dal.tinyir.alerts.ScoredResult
import ch.ethz.dal.tinyir.indexing.{InvertedIndex, SimpleIndex, SimpleResult}
import ch.ethz.dal.tinyir.io.{DocStream, TipsterStream}
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.processing.{Document, Tokenizer, XMLDocument}


type DocRef = SimpleResult

type DocIndex = SimpleIndex

// Scored Document, on purpose permuted compare member to have descending sort order
case class ScoredDocument(val id: Int, val score : Double) extends Ordered[ScoredDocument] {
  def compare(that: ScoredDocument) = that.score compare this.score
}


  // ************************ Search engine **************************

/** Two-step search engine retrieving all candidates from an inverted index
  * and evaluating score on them subsequently
  */
abstract class ScoreBasedSearchEngine(docIndex : DocIndex) {
  protected val index : DocIndex = docIndex

  def search(query : Set[String]) : List[ScoredDocument] = {
    rank(query,query.flatMap(index.results))
  }

  def rank(query : Set[String], candidates : Set[DocRef]) : List[ScoredDocument]
}


// ************************ Tf-Idf model **************************

/** Stores TF and parameters for IDF and scores a given set of candidates
  */
class TfIdfModel(docStream : Stream[Document], docIndex : DocIndex) extends ScoreBasedSearchEngine(docIndex) {

  // Per term document frequency
  private val docFrequency : Map[String,Int] = index.index.mapValues( _._2.length )

  // Total number of documents TODO: this is badly implemented
  private val numDocs : Double = /*docIndex.numDocs*/ index.index.flatMap(_._2.map(posting => posting.id)).toList.distinct.size

  // Document-wise term frequencies
  private val docTermFrequencies : Map[DocRef, Map[String,Int]]  = {
    var tf = collection.mutable.Map[DocRef,Map[String,Int]]()
    for (doc <- docStream) {
      tf += doc.ID -> Tokenizer.tokenize(doc.content).groupBy(identity).mapValues(_._2.length)
    }
  }


  /** Ranks a set of candidate documents according to TF-IDF term-model scoring criterion
    *
    * @param query       The query
    * @param candidates  Candidate documents with at least one word in common with a document in the collection
    * @return
    */
  override def rank(query : Set[String], candidates : Set[DocRef]) : List[ScoredDocument] = {
    candidates.mapValues(doc => query.map(TfIdfModel.tfIdf(docTermFrequencies.get(doc).get(_),
                                           docFrequency.get(_), numDocs))
                                     .sum).toList.sort
  }
}



object TfIdfModel {
  /** The TF-IDF scoring function implementation
    *
    * @param termFrequency
    * @param documentFrequency
    * @param numDocs
    * @return                   TF-IDF score
    */
  def tfIdf(termFrequency: Double, documentFrequency : Double, numDocs : Double) : Double = {
    if (termFrequency > 0) {
      return TermFrequencies.log2( 1. + termFrequency ) * TermFrequencies.log2(numDocs/documentFrequency))
    } else {
      return 0
    }
  }
}


// ************************ Benchmarking **************************

// TODO: Implementation and test data parsing
object SearchBenchmark {

  // def precision (TBD)

  // def recall (TBD)

  // def F1 = 2.*precision*recall/(precision + recall)

  // average precision average of sums of precision up to every relevant document divided by number of relevant documents
  // get code in tinyIR

  // mean average precision: query-average precision averaged over all queries

}




// ************************ Main function **************************

class ProjectMain {
  def main(argv : Array[String]) : Int = {
    val docStream = new TipsterStream("./data/documents")

    val docIndex : SimpleIndex = new SimpleIndex(new TipsterStream("./data/documents"))

    val tfIdfSearch : ScoreBasedSearchEngine = new TfIdfModel(docStream, docIndex)

    // languageModelSearch : ScoreBasedSearchEngine = new LanguageModel(docStream, docIndex)

    // output results on queries
    val queryString = "Some random query words "
    println(queryString + " give results:")
    for(scoredDoc <- tfIdfSearch.search(Set("Some","random","query", "words")).take(100)) {
      println ("  "  + scoredDoc.id.toString + " : " + scoredDoc.score.toString)
    }
    // languageModelSearch.search(new Set[String]("Some","random","words")).take(100)

    // TODO: Check against provided test data
  }
}


