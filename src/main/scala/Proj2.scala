import Typedefs._
import math.min
import collection.mutable

/** Main object of this application.
  * Constructs an inverted and forward index used to build
  * term- as well as language-based models. Then performs
  * search for queries and evalutes the obtained rankings
  * against a priori known relevance results.
  */
object Proj2 {
  val DATAPATH = "./data/documents"
  val QUERYPATH = "./data/questions-descriptions.txt"
  val JUDGEPATH = "./data/relevance-judgements.csv"

  val numSearchResultsReqested = 100



  def printOutResult(index: DocIndex, result: Map[QueryId, List[ScoredDocument]]): Unit = {
    val numSearchResultsReq : Int = numSearchResultsReqested

    result.toList.sortBy(_._1).foreach{ case (qnum, resultlist) =>
      val finallist = if (resultlist.size < numSearchResultsReq) resultlist.map(_.doc) ++ index.fillerDocs.take(numSearchResultsReq-resultlist.size) else resultlist

      for( i <- 1 to finallist.size) {
        println(qnum + " " + i + " " + finallist(i-1))
      }
    }

    println("Queries with less than " + numSearchResultsReq +" returned results from relevance model: " + result.filter(_._2.size < numSearchResultsReq).size)
    result.filter(_._2.size < numSearchResultsReq).foreach{ case (query,rankedList) => println(query + " : " + rankedList.size + " results.") }
  }

  def main(args: Array[String]): Unit = {

    // ***** Build indices *****

    // Process only a fraction of the collection (used for rapid prototyping)
    val fraction : Double = 0.2

    val numSearchResults : Int = numSearchResultsReqested

    println("***** Building Indices... *****")
    val tBeforeIndexConstruction = System.nanoTime();
    val index = new DocIndex( Proj2.DATAPATH, numSearchResults, fraction )
    val tAfterIndexConstruction = System.nanoTime();
    val tIndexConstruction = (tAfterIndexConstruction-tBeforeIndexConstruction).toDouble/1e6
    println("***** Built indices (" + tIndexConstruction + " ms) *****")


    // ***** Load search queries and relevance data *****

    val queries = Query.load(Proj2.QUERYPATH)
    val judge : Map[QueryId,List[DocId]] = RelevanceJudge.load(Proj2.JUDGEPATH)


    // ***** Create language model *****

    //val lm = new LanguageModel(index)
    val maxLhLM = new LanguageModel(index,numSearchResults)


    // ***** Perform search *****

    println("***** Language model: Start fast search *****")
    // val lmresult = queries.mapValues{lm.predict}.mapValues(_.map(_._1))
    val tBeforeLMFastSearch = System.nanoTime();
    val lmfResult = maxLhLM.fastSearch(queries)
//    println("***** Language model: Number of fast search results: ******")
//    lmfResult.foreach{ case (query,rankedList) => println(query + " : " + rankedList.length ) }
//    lmfResult.foreach(println)
    println("***** Language model: Fast search results *****")
    printOutResult(index, lmfResult)
    val tAfterLMFastSearch = System.nanoTime();
    val tLMFastSearch = (tAfterLMFastSearch - tBeforeLMFastSearch ).toDouble/1e6
    println("***** Language model: Fast search finished (" + tLMFastSearch + " ms) *****")

    // ***** Evaluate search results *****

    println("***** Evaluating Language model *****")
    val evalSearchLm = EvaluateRanking.create(lmfResult, judge)
    evalSearchLm.judgement
    println("***** Evaluation of language model finished *****")

    println("***** Language model: Start slow search *****")
    val tBeforeLMSlowSearch = System.nanoTime();
    val lmsResult = maxLhLM.slowSearch(queries)
//    println("***** Language model: Number of slow search results: "+lmsResult.size)
//    lmsResult.foreach(println)
    println("***** Language model: Slow search results *****")
    printOutResult(index, lmsResult)
    val tAfterLMSlowSearch = System.nanoTime();
    val tLMSlowSearch = (tAfterLMSlowSearch - tBeforeLMSlowSearch ).toDouble/1e6
    println("***** Language model: Slow search finished (" + tLMSlowSearch + " ms) *****")


    // ***** Create term model *****

    val tfIdfM = new TfIdfModel(index,numSearchResults)

    // ***** Perform search *****

    println("***** Term model: Start search *****")
    val tBeforeTMFastSearch = System.nanoTime();
    val tmfResult = tfIdfM.fastSearch(queries)
//    println("***** Term model: Number of fast search results: ******")
//    tmfResult.foreach{ case (query,rankedList) => println(query + " : " + rankedList.length ) }
//    tmfResult.foreach(println)
    println("***** Term model: Fast search results *****")
    printOutResult(index, tmfResult)
    val tAfterTMFastSearch = System.nanoTime();
    val tTMFastSearch = (tAfterTMFastSearch - tBeforeTMFastSearch ).toDouble/1e6
    println("***** Term model search finished (" + tTMFastSearch + " ms) *****")


    // ***** Evaluate search results *****

    println("***** Evaluating term model model *****")
    val evalSearchTM = EvaluateRanking.create(tmfResult, judge)
    evalSearchTM.judgement
    println("***** Evaluation of term model finished *****")


    println("***** Term model: Start slow search *****")
    val tBeforeTMSlowSearch = System.nanoTime();
    val tmsResult = tfIdfM.slowSearch(queries)
//    println("***** Term model: Number of slow search results: "+tmsResult.size)
//    tmsResult.foreach(println)
    println("***** Term model: Slow search results *****")
    printOutResult(index, tmsResult)
    val tAfterTMSlowSearch = System.nanoTime();
    val tTMSlowSearch = (tAfterTMSlowSearch - tBeforeTMSlowSearch).toDouble/1e6
    println("***** Term model: Slow search finished (" + tTMSlowSearch + " ms) *****")


    println("***** Execution times *****")
    println("Index construction (ms):         " + tIndexConstruction )
    println("Language model fast search (ms): " + tLMFastSearch )
    println("Language model slow search (ms): " + tLMSlowSearch )
    println("Term model fast search (ms):     " + tTMFastSearch )
    println("Term model slow search (ms):     " + tTMSlowSearch )

  }

}
