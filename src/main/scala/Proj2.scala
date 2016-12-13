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



  def printOutResult(index: DocIndex, result: Map[QueryId, List[ScoredDocument]]): Unit = {
    val shortresult = mutable.Set[QueryId]()
    result.toList.sortBy(_._1).foreach{ case (qnum, resultlist) =>
      var newlist = List[String]()
      if (resultlist.size < 100) {
        shortresult.add(qnum)
        val doclist = index.fwIndex
                           .toList
                           .sortBy(_._2.map(_._2).sum)
                           .map(_._1)
                           .take(100 - resultlist.size)
        newlist = doclist
      }
      val finallist = resultlist.map(_.doc) ++ newlist

      for( i <- 1 to finallist.size) {
        println(qnum + " " + i + " " + finallist(i-1))
      }
    }
    println("queries has result less than 100" + shortresult)
  }

  def main(args: Array[String]): Unit = {

    // ***** Build indices *****

    // NOTE: We construct the TipsterStream on the stack in DocIndex constructor
    // so it doesn't outlive long beyond index construction

    // Process only a fraction of the collection
    val fraction : Double = 0.1

    println("***** Building Indices... *****")
    val tBeforeIndexConstruction = System.nanoTime();
    val index = new DocIndex( Proj2.DATAPATH, fraction )
    val tAfterIndexConstruction = System.nanoTime();
    val tIndexConstruction = (tAfterIndexConstruction-tBeforeIndexConstruction).toDouble/1e6
    println("***** Built indices (" + tIndexConstruction + " ms) *****")


    // ***** Load search queries and relevance data *****

    val queries = Query.load(Proj2.QUERYPATH)
    val judge : Map[QueryId,List[DocId]] = RelevanceJudge.load(Proj2.JUDGEPATH)

    val numSearchResults : Int = 100


    // TODO: move this to extra function

    // ***** Create language model *****

    //val lm = new LanguageModel(index)
    val maxLhLM = new NewLanguageModel(index,numSearchResults)

    // TODO: Train ("tune") model parameters

    // ***** Perform search *****

    println("***** Start Language model search *****")
    val tBeforeLMFastSearch = System.nanoTime();
    val lmfResult = maxLhLM.fastSearch(queries)
    println("***** Language model search results *****")
    printOutResult(index, lmfResult)
    val tAfterLMFastSearch = System.nanoTime();
    val tLMFastSearch = (tAfterLMFastSearch - tBeforeLMFastSearch ).toDouble/1e6
    println("***** Language model search finished (" + tLMFastSearch + " ms) *****")

    // ***** Evaluate search results *****

    println("***** Evaluating Language model *****")
    val evalSearchLm = EvaluateRanking.create(lmfResult, judge)
     evalSearchLm.judgement
    println("***** Evaluation of language model finished *****")

    println("***** Language model: Start slow search *****")
    val tBeforeLMSlowSearch = System.nanoTime();
    val lmsResult = maxLhLM.slowSearch(queries)
    println("***** Language model: Slow search results *****")
    printOutResult(index, lmsResult)
    val tAfterLMSlowSearch = System.nanoTime();
    val tLMSlowSearch = (tAfterLMSlowSearch - tBeforeLMSlowSearch ).toDouble/1e6
    println("***** Language model: Slow search finished (" + tLMSlowSearch + " ms) *****")


    // ***** Create term model *****

    val tfIdfM = new TfIdfModel(index,numSearchResults)

    // ***** Perform search *****

    println("***** Start term model search *****")
    val tBeforeTMFastSearch = System.nanoTime();
    val tmfResult = tfIdfM.fastSearch(queries)
    println("***** Term model search results *****")
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
