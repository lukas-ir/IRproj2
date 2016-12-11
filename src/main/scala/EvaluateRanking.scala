import Typedefs._
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth

import scala.math.min

/** Creates judge
  */
object RelevanceJudge {
  def load(path: String): Map[QueryId, List[DocId]] =
    new TipsterGroundTruth(path).judgements.map { case (k, v) => (k.toInt, v.toList) }
}

/** Search ranking evaluation
  * Includes Precision, Recall, bounded average precision, mean bounded average precision
  */
class EvaluateRanking(retrieved:Map[QueryId, List[ScoredDocument]], relevant: Map[QueryId, List[DocId]]) {
  val TP : Map[QueryId, Set[DocId]] = {
    retrieved.map{ case (qnum, retr) =>
      (qnum, retr.map(_.doc).intersect(relevant(qnum)).toSet)
    }
  }

  val FP : Map[QueryId, Set[DocId]] = {
    retrieved.map{ case (qnum, retr) =>
      (qnum, retr.map(_.doc).toSet -- TP(qnum))
    }
  }

  val FN : Map[QueryId, Set[DocId]] = {
    relevant.map{case (qnum, relv) =>
      (qnum, relv.toSet -- TP(qnum))
    }
  }

  val Precision : Map[QueryId, Double] = {
    TP.map{ case (qnum, tplist) =>
      (qnum,
        if (min(relevant(qnum).size, retrieved(qnum).size) > 0)
          tplist.size.toDouble / min(relevant(qnum).size, retrieved(qnum).size)
        else if (retrieved(qnum).size == 0)
          0.0
        else /* relevant(qnum).size == 0 */
          1.0
        )
    }
  }

  val Recall : Map[QueryId, Double] = {
    TP.map{ case (qnum, tplist) =>
      (qnum, if (relevant(qnum).size > 0) tplist.size.toDouble / relevant(qnum).size else 1.0)
    }
  }

  val F1 : Map[QueryId, Double] = retrieved.map {
    case (qnum, _) => {
      val precision = Precision(qnum)
      val recall = Recall(qnum)
      if (precision == 0.0 || recall == 0.0)
        qnum -> 0.0
      else
        qnum -> 2.0 * precision * recall / (precision + recall)
    }
  }

  // Bounded average precision
  val AP : Map[QueryId, Double] = retrieved.map {
    case (qnum, retr) => {
      val precision = Precision(qnum)
      val recall = Recall(qnum)
      assert( min(relevant(qnum).size, retr.size) > 0) /* this case needs special treatment (either AP = 1 or 0)*/
      qnum -> retr.sorted
                  .zipWithIndex /* zip with retrieved index */
                  .filter(retrIndex => TP(qnum).contains(retrIndex._1.doc))
                  .sortBy(_._2)
                  .zipWithIndex /* zip with index among true positives */
                  .map { case (docIndex, tpIndex) => (tpIndex + 1).toDouble / (docIndex._2 + 1).toDouble }.sum / min(relevant(qnum).size, retr.size).toDouble
    }
  }

  // Query-mean average precision
  val MAP : Double = AP.foldLeft(0.0)( _ + _._2 ) / AP.size.toDouble

  /* // Yi's implementation
  val AP = {
    retrieved.map { case (qnum, _) =>
      (qnum, {
        var patk = 0.0
        val rele = relevant(qnum)
        val retv = retrieved(qnum)
        for (i <- 1 to retv.size) {
          if (rele.contains(retv(i-1))) {
            val tp = retv.slice(0, i).intersect(rele).size
            patk += tp.toDouble / i
          }
        }
        patk / (TP(qnum).size + FN(qnum).size)
      })
    }
  }

  val MAP = AP.values.sum / AP.size

  val F1 = Precision.map{ case (qnum, pr) =>
    val rc = Recall(qnum)
    (qnum, 2.0 * pr * rc / (pr + rc))
  }
  */

  val judgement = {
    for (qnum <- relevant.keySet.toList.sorted) {
      println("Query: " + qnum)
      println("TP: "+TP(qnum).size+" FP: "+FP(qnum).size+" FN: " + FN(qnum).size)
      println("Precision: "+Precision(qnum)+" Recall: "+Recall(qnum) + " F1-score:" + F1(qnum))
      println("AP: "+AP(qnum))
      println(retrieved(qnum))
      println("-----------------------------------------")
    }
    println("MAP: "+MAP)
    println("-----------------------------------------")
    true
  }
}

object EvaluateRanking {
  def create(searchResults : Map[QueryId,List[ScoredDocument]], judge : Map[QueryId,List[DocId]]) : EvaluateRanking = {
    new EvaluateRanking(searchResults/*.mapValues(_.map(_.doc))*/,judge) // TODO: Check that this list is not reordered
  }

  def main(args : Array[String]) = {

    val retrieved = Map[QueryId,List[ScoredDocument]](
      1 -> List[ScoredDocument](
        ScoredDocument("23",3.4),
        ScoredDocument("27",2.4),
        ScoredDocument("13",1.7),
        ScoredDocument("43",1.1)),
      5 -> List[ScoredDocument](
        ScoredDocument("22",113.9),
        ScoredDocument("21",31.24),
        ScoredDocument("202",30.1),
        ScoredDocument("19",1.4)),
      7 -> List[ScoredDocument](
        ScoredDocument("53",13.8),
        ScoredDocument("117",12.4),
        ScoredDocument("13",1.0),
        ScoredDocument("33",0.5))
    )

    val relevant = Map[QueryId,List[DocId]](
      1 -> List[DocId]("29","23","7","49","45"),
      5 -> List[DocId]("23","202","23","35","76"),
      7 -> List[DocId]("117","33"))

    val eval = new EvaluateRanking(retrieved,relevant)
    eval.judgement

    // Exact values
    val qnums = List(1,5,7)

    val exactPrec = Map[QueryId,Double](
      1 -> 1.0/4.0,
      5 -> 1.0/4.0,
      7 -> 2.0/2.0
    )

    val exactRecall = Map[QueryId,Double](
      1 -> 1.0/5.0,
      5 -> 1.0/5.0,
      7 -> 2.0/2.0
    )

    val f1 = (precision : Double, recall : Double ) => 2.0 * precision * recall / (precision + recall)

    val exactF1 : Map[QueryId,Double] =
      qnums.map( qnum => qnum -> f1(exactPrec(qnum),exactRecall(qnum)) ).toMap

    val exactAP = Map[QueryId,Double](
      1 -> (1.0/1.0          )/4.0,
      5 -> (1.0/3.0          )/4.0,
      7 -> (1.0/2.0 + 1.0/2.0)/2.0
    )

    val exactMAP = exactAP.foldLeft(0.0)( _ + _._2 )/exactAP.size

    val exactUpToEps = (x : Double, y : Double) => math.abs(x-y) < 1e-14

    eval.Precision.foreach{ case (qnum,value) => assert(exactUpToEps(value, exactPrec(qnum))) }
    eval.Recall.foreach{ case (qnum,value) => assert(exactUpToEps(value, exactRecall(qnum))) }
    eval.F1.foreach{ case (qnum,value) => assert(exactUpToEps(value, exactF1(qnum))) }
    eval.AP.foreach{ case (qnum,value) => assert(exactUpToEps(value, exactAP(qnum))) }
    assert( exactUpToEps(eval.MAP, exactMAP) )

  }

}


