import scala.math.min

/** Search ranking evaluation
  * Includes Precision, Recall, bounded average precision, mean bounded average precision
  */
class EvaluateRanking(retrieved:Map[Int, List[String]], relevant: Map[Int, List[String]]) {
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


  // TODO: F1, average precision, mean average precision

  // def F1 = 2.*precision*recall/(precision + recall)

  // average precision average of sums of precision up to every relevant document divided by number of relevant documents
  // get code in tinyIR

  // mean average precision: query-average precision averaged over all queries


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