import Typedefs._

import ch.ethz.dal.tinyir.io.TipsterStream
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

    val docStream = new TipsterStream(DATAPATH)


    // Process only a fraction of the collection
    val fraction : Double = 0.001
    val docStreamPartition = TipsterStreamPartition.create(docStream,fraction)


    // TODO: Try to build the indices non-lazily so we can attribute performance to their construction
    println("***** Building Indices... *****")
    val index = new DocIndex( docStreamPartition.stream )
    println("***** Built indices. *****")


    // ***** Load search queries and relevance data *****

    val queries = Query.load(QUERYPATH)
    val judge = new TipsterGroundTruth(JUDGEPATH).judgements.map{case (k,v)=>(k.toInt, v.toList)}


    // ***** Create language model *****

    //val lm = new LanguageModel(index)
    val maxLhLM = new NewLanguageModel(index)

    // TODO: Train ("tune") model parameters

    // ***** Perform search *****

    println("***** Start Language model search *****")
    // val lmresult = queries.mapValues{lm.predict}.mapValues(_.map(_._1))
    val lmResult = maxLhLM.search(queries,100).mapValues(_.map(_.doc))
    println("***** Language model search finished *****")

    // ***** Evaluate search results *****

    println("***** Evaluating Language model *****")

    val evalSearchLm = new EvaluateRanking(lmResult, judge)
    evalSearchLm.judgement

    println("***** Evaluation of language model finished *****")



    // ***** Create term model *****

    val tfIdfM = new TfIdfModel(index)

    // ***** Perform search *****

    println("***** Start term model search *****")
    val tmResult = tfIdfM.search(queries,100).mapValues(_.map(_.doc))
    println("***** Term model search finished *****")

    // ***** Evaluate search results *****

    println("***** Evaluating term model model *****")

//    val evalSearchTM = new EvaluateRanking(tmResult, judge)
//    evalSearchTM.judgement

    println("***** Evaluation of term model finished *****")

  }
}
