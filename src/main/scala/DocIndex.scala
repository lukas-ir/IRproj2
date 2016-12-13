import Typedefs._

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument

import collection.mutable


case class TfTuple(term: Term, doc : DocId, count: Int)

object DocIndexHelper {
  def tfStream(path : String) : Stream[TfTuple] = {
    val docStream = new TipsterStream(path)
    //    val docStream = new TipsterStreamSubsample(path,fraction)
    docStream.stream.flatMap{ doc =>
      Tokenizer.tokenize(doc.content)
        .groupBy(identity)
        //               .filter(_._2.length > 3)
        .map{case (tk,fq) => TfTuple(tk.intern(), doc.name.intern(), fq.length)}
    }
  }

}


/** Provides both inverted and forward indices
  *
  * @param path path of the Tipster data set
  * @param fraction share of the Tipster data set to analyze
  */
class DocIndex(path: String, fraction : Double){


  // TODO: Rename to invIndex
  /** Inverted index
    * Maps token -> (document ID containing this token ->  token frequency in this document)
    */
  val fqIndex : Map[Term, Map[DocId, Int]] = {
    val map = mutable.Map[Term, mutable.ListBuffer[(DocId, Int)]]()
    for (tftuple <- DocIndexHelper.tfStream(path)) {
      map.getOrElseUpdate(tftuple.term, mutable.ListBuffer[(DocId, Int)]())
      map(tftuple.term) += ((tftuple.doc.intern(), tftuple.count))
    }
    map
       .filter(_._2.size > 2) // choose terms appear in at least 3 docs
       .filter(_._2.size < 6000)
       .mapValues(_.toMap)
       .toMap
  }

  /** Forward index
    * Maps document -> (contained token -> token frequency in this document)
    */
  val fwIndex : Map[DocId, Map[Term, Int]] = {
    val map = mutable.Map[DocId, mutable.ListBuffer[(Term, Int)]]()
    for ((tk, docfreqmap) <- fqIndex) {
      for ((doc, freq) <- docfreqmap) {
        map.getOrElseUpdate(doc.intern(), mutable.ListBuffer[(Term, Int)]())
        map(doc) += ((tk, freq))
      }
    }
    map
        .mapValues(_.toMap)
        .toMap
  }

  /** TODO: Map to word-frequency and collection frequency
    */
  case class DfCfTuple(val df : Int, val cf : Int)

  val dcInvIndex : Map[Term,DfCfTuple] =
    fqIndex.mapValues( docFreqMap => DfCfTuple(docFreqMap.size,docFreqMap.foldLeft(0)(_ + _._2)) )


  //************************************************************

  // TODO: Everything below here should be removed and replaced by calls to the above three indices...

  //************************************************************


  // TODO: Consider moving to language model
  /* doc -> ntoken of this doc*/
  lazy val ntokensdoc = fwIndex.mapValues(_.values.sum)

  /* total number of tokens in the collection */
  lazy val ntokens = ntokensdoc.foldLeft(0)(_ + _._2)

  // TODO: Move everything to language model, what belongs there
  // Witten Bell Smoothing
//  lazy val lambdad = ntokensdoc.mapValues(1/_.toDouble)
  lazy val lambdad = fwIndex.map{ case (doc, tfmap) =>
    val cf = tfmap.values.sum
    (doc, cf.toDouble / (cf + fwIndex.size).toDouble)
  }

  // TODO: Seems to be unused
  lazy val docList = fwIndex.keySet.toList

  // TODO: Seems to be unused
  lazy val vocab = fqIndex.keySet.toList

  // TODO: Consider moving to language model
  lazy val pw = fqIndex.mapValues(_.values.sum.toDouble / ntokens)
}


/** DocIndex test
  * Currently runs with only a fraction of the full document collection
  */
object DocIndexTest {
  def main(args: Array[String]): Unit = {
    val fname = "./data/documents"

    val fraction : Double = 0.1

    val docs = new DocIndex(fname, fraction)

    val dist = docs.fqIndex.mapValues(_.size).groupBy(_._2).mapValues(_.map(_._1))
    for (i <- dist.keySet.toList.sorted) {

      println(i, dist(i).size, dist(i))
    }
  }
}