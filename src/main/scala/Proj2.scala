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
    println("Building Indices... ")
    val index = new DocIndex( docStream/*Partition*/.stream )
    println("Built indices.")


    // ***** Create relevance models *****

    //val lm = new LanguageModel(index)
    val maxLhLM = new NewLanguageModel(index)

    // TODO: Tf-Idf model

    // TODO: Train ("tune") models


    // ***** Perform search *****

    val queries = Query.load(QUERYPATH)

    println("Start Language model search")
    // val lmresult = queries.mapValues{lm.predict}.mapValues(_.map(_._1))

    //val lmresult = queries.mapValues{lm.predict}.mapValues(_.map(_._1))
    val lmresult = maxLhLM.search(queries,100).mapValues(_.map(_.doc))
    println("Language model search finished")


    // ***** Evaluate search results *****

    val judge = new TipsterGroundTruth(JUDGEPATH).judgements.map{case (k,v)=>(k.toInt, v.toList)}
    val evalSearch = new EvaluateRanking(lmresult, judge)
    evalSearch.judgement


  }
}
