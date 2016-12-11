import Typedefs._

import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth

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


  def main(args: Array[String]): Unit = {

    // ***** Build indices *****

    // NOTE: We construct the TipsterStream on the stack in DocIndex constructor
    // so it doesn't outlive long beyond index construction

    // Process only a fraction of the collection
    val fraction : Double = 0.3

    println("***** Building Indices... *****")
    val index = new DocIndex( DATAPATH, fraction )
    println("***** Built indices. *****")


    // ***** Load search queries and relevance data *****

    val queries = Query.load(QUERYPATH)
    val judge : Map[QueryId,List[DocId]] = RelevanceJudge.load(JUDGEPATH)

    val numSearchResults : Int = 100


    // TODO: move this to extra function

    // ***** Create language model *****

    //val lm = new LanguageModel(index)
    val maxLhLM = new NewLanguageModel(index,numSearchResults)

    // TODO: Train ("tune") model parameters

    // ***** Perform search *****

    println("***** Start Language model search *****")
    // val lmresult = queries.mapValues{lm.predict}.mapValues(_.map(_._1))
    val lmResult = maxLhLM.search(queries)
    println("***** Language model search finished *****")

    // ***** Evaluate search results *****

    println("***** Evaluating Language model *****")

    val evalSearchLm = EvaluateRanking.create(lmResult, judge)
    evalSearchLm.judgement

    println("***** Evaluation of language model finished *****")



    // ***** Create term model *****

    val tfIdfM = new TfIdfModel(index,numSearchResults)

    // ***** Perform search *****

    println("***** Start term model search *****")
    val tmResult = tfIdfM.search(queries)
    println("***** Term model search finished *****")

    // ***** Evaluate search results *****

    println("***** Evaluating term model model *****")

    val evalSearchTM = EvaluateRanking.create(tmResult, judge)
    evalSearchTM.judgement

    println("***** Evaluation of term model finished *****")

  }



}
