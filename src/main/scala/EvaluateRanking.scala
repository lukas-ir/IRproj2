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