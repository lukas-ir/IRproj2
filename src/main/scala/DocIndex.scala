import ch.ethz.dal.tinyir.io.{ParsedXMLStream, TipsterStream}
import ch.ethz.dal.tinyir.processing.XMLDocument


import math.ceil
import collection.mutable
import math.log



object TipsterStreamPartition {

  /** Creates a random partition of 0 to maxId
    *
    * @param fraction
    * @param maxId
    */
  private def generatePartitionIds(maxId : Int, fraction: Double): Set[Int] = {
    val partitionSize = ceil(fraction * maxId)
    val resultPartition = new mutable.HashSet[Int]()
    while (resultPartition.size < partitionSize) {
      resultPartition += util.Random.nextInt(maxId)
    }
    resultPartition.toSet
  }

  def create(parentStream: TipsterStream, fraction : Double): TipsterStreamPartition = {
    return new TipsterStreamPartition(parentStream,generatePartitionIds(parentStream.length, fraction))
  }
}

/** Creates a partition of a TipsterSteam
  *
  * @param parentStream
  * @param partitionIds
  */
case class TipsterStreamPartition(val parentStream: TipsterStream, val partitionIds : Set[Int]) {
  def stream : Stream[XMLDocument] = parentStream.stream.zipWithIndex.filter { case (doc,id) => partitionIds(id) }.map(_._1)
  def length : Int = partitionIds.size
}



class DocIndex(docStream: Stream[XMLDocument]){

  private case class TfTuple(term: String, doc : String, count: Int)

  private def TfStream : Stream[TfTuple] = {
    var count = 0
    docStream.flatMap{ doc =>
      Tokenizer.tokenize(doc.content)
               .groupBy(identity)
//               .filter(_._2.length > 3)
               .map{case (tk,fq) => TfTuple(tk.intern(), doc.name.intern(), fq.length)}
    }
  }



  /* token -> list of docID containing this token and its frequency */
  lazy val fqIndex : Map[String, Map[String, Int]] = {
    val map = mutable.Map[String, mutable.ListBuffer[(String, Int)]]()
    for (tftuple <- TfStream) {
      map.getOrElseUpdate(tftuple.term, mutable.ListBuffer[(String, Int)]())
      map(tftuple.term) += ((tftuple.doc.intern(), tftuple.count))
    }
    map
       .filter(_._2.size > 2) // choose terms appear in at least 3 docs
       .filter(_._2.size < 6000)
       .mapValues(_.toMap)
       .toMap
  }

  /* document -> list of tokens and its frequency */
  lazy val fwIndex : Map[String, Map[String, Int]] = {
    val map = mutable.Map[String, mutable.ListBuffer[(String, Int)]]()
    for ((tk, docfreqmap) <- fqIndex) {
      for ((doc, freq) <- docfreqmap) {
        map.getOrElseUpdate(doc.intern(), mutable.ListBuffer[(String, Int)]())
        map(doc) += ((tk, freq))
      }
    }
    map
        .mapValues(_.toMap)
        .toMap
  }

  /* doc -> ntoken of this doc*/
  lazy val ntokensdoc = fwIndex.mapValues(_.values.sum)

  /* total number of tokens in the collection */
  lazy val ntokens = ntokensdoc.foldLeft(0)(_ + _._2)

  lazy val lambdad = ntokensdoc.mapValues(1/_.toDouble)

  lazy val docList = fwIndex.keySet.toList

  lazy val vocab = fqIndex.keySet.toList

  lazy val pw = fqIndex.mapValues(_.values.sum.toDouble / ntokens)
}

object test {
  def main(args: Array[String]): Unit = {
    val fname = "./data/documents"
    val docStream = new TipsterStream(fname)

    val fraction : Double = 0.1
    val docStreamPartition = TipsterStreamPartition.create(docStream,fraction)


    val docs = new DocIndex(docStreamPartition.stream)



    val dist = docs.fqIndex.mapValues(_.size).groupBy(_._2).mapValues(_.map(_._1))
    for (i <- dist.keySet.toList.sorted) {

      println(i, dist(i).size, dist(i))
    }
  }
}