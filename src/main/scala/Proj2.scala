import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth

import scala.io.Source
import math.min

class Evaluate(retrieved:Map[Int, List[String]], relevant: Map[Int, List[String]]) {
  val TP = {
    retrieved.map{ case (qnum, retr) =>
      (qnum, retr.intersect(relevant(qnum)).toSet)
    }
  }

  val FP = {
    retrieved.map{ case (qnum, retr) =>
      (qnum, retr.toSet -- TP(qnum))
    }
  }

  val FN = {
    relevant.map{case (qnum, relv) =>
      (qnum, relv.toSet -- TP(qnum))
    }
  }

  val Precision = {
    TP.map{ case (qnum, tplist) =>
      (qnum, tplist.size.toDouble / retrieved(qnum).size)
    }
  }

  val Recall = {
    TP.map{ case (qnum, tplist) =>
      (qnum, tplist.size.toDouble / min(relevant(qnum).size, 100.0))
    }
  }


  def judgement = {
    for (qnum <- relevant.keySet.toList.sorted) {
      println("Query: " + qnum)
      println("TP: "+TP(qnum).size+" FP: "+FP(qnum).size+" FN: " + FN(qnum).size)
      println("Precision: "+Precision(qnum)+" Recall: "+Recall(qnum))
      println(retrieved(qnum))
      println("-----------------------------------------")
    }
  }
}

object Proj2 {
  val DATAPATH = "./data/documents"
  val QUERYPATH = "./data/questions-descriptions.txt"
  val JUDGEPATH = "./data/relevance-judgements.csv"

  def loadQuery(path: String) =
    Source.fromFile(path).getLines()
      .filter(_.matches(".*<num>.*|.*<title>.*"))
      .map(_.split("[ ]+", 3).slice(2,3)(0).mkString("").trim())
      .grouped(2).map(q=>(q(0).toInt, q(1)))
      .toMap


  def main(args: Array[String]): Unit = {
    val querys = loadQuery(QUERYPATH)
    val docStream = new TipsterStream(DATAPATH)

    /*
    // Process only a fraction of the collection
    val fraction : Double = 0.1
    val docStreamPartition = TipsterStreamPartition.create(docStream,fraction)
    */

    val index = new DocIndex( docStream/*Partition*/.stream )
    val judge = new TipsterGroundTruth(JUDGEPATH).judgements.map{case (k,v)=>(k.toInt, v.toList)}

    val lm = new LanguageModel(index)
    println("Start Predicting")
    val lmresult = querys.mapValues{lm.predict}.mapValues(_.map(_._1))


//    println(lm.predict("Airbus Subsidies"))


//    for (qnum <- lmresult.keySet.toList.sorted){
//      println(qnum)
//    }
    println("Prediction finished")

    val eva = new Evaluate(lmresult, judge)
    eva.judgement
  }
}
