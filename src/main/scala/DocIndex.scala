import Typedefs._

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument

import collection.mutable


/** Provides inverted index from (preprocessed) tokens to document IDs
  *
  * @param path path of the Tipster data set
  * @param fraction share of the Tipster data set to analyze
  *
  * TODO: Instead of just aggregating huge data structures, maybe offer an interface
  */
class DocIndex(path: String, fraction : Double){

  private case class TfTuple(term: Term, doc : DocId, count: Int)

  private def tfStream : Stream[TfTuple] = {
    //val docStream = new TipsterStream(path)
    val docStream = new TipsterStreamSubsample(path,fraction)
    docStream.stream.flatMap{ doc =>
      Tokenizer.tokenize(doc.content)
               .groupBy(identity)
//               .filter(_._2.length > 3)
               .map{case (tk,fq) => TfTuple(tk.intern(), doc.name.intern(), fq.length)}
    }
  }



  // TODO: maybe rename fqIndex - does this abbrevation have a meaning?
  /* Nested map: token -> (document ID containing this token ->  token frequency in this document) */
  val fqIndex : Map[Term, Map[DocId, Int]] = {
    val map = mutable.Map[Term, mutable.ListBuffer[(DocId, Int)]]()
    for (tftuple <- tfStream) {
      map.getOrElseUpdate(tftuple.term, mutable.ListBuffer[(DocId, Int)]())
      map(tftuple.term) += ((tftuple.doc.intern(), tftuple.count))
    }
    map
       .filter(_._2.size > 2) // choose terms appear in at least 3 docs FIXME: Why do we remove the most specific terms?
       .filter(_._2.size < 6000)  // TODO: Make thsi a parameter based on the number of search results
       .mapValues(_.toMap)
       .toMap
  }

  /* Nested map: document -> (contained token -> token frequency in this document) */
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



  // TODO: Consider moving to language model
  /* doc -> ntoken of this doc*/
  lazy val ntokensdoc = fwIndex.mapValues(_.values.sum)

  /* total number of tokens in the collection */
  lazy val ntokens = ntokensdoc.foldLeft(0)(_ + _._2)

  // TODO: Move everything to language model, what belongs there
  lazy val lambdad = ntokensdoc.mapValues(1/_.toDouble)

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