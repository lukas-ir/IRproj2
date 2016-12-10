import scala.math.min

/** Search ranking evaluation
  *
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