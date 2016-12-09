import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer

import collection.mutable



object Tokenizer {
  def tokenize(content: String) = {
    val tokens = content.toLowerCase()
                        .replaceAll("[^a-z ]", " ")
                        .split(" ")
                        .filter(_.length > 3)

    StopWords.filterOutSW(tokens)
             .map(PorterStemmer.stem)

  }
}

class DocIndex(filename: String){

  private case class TfTuple(term: String, doc : String, count: Int)

  private def TfStream : Stream[TfTuple] = {
    var count = 0
    val in = new TipsterStream(filename)
    in.stream.flatMap{ doc =>
      Tokenizer.tokenize(doc.content)
               .groupBy(identity)
               .map{case (tk,fq) => TfTuple(tk.intern(), doc.name.intern(), fq.length)}
    }
  }



  /* token -> list of docID containing this token and its frequency */
  lazy val fqIndex : Map[String, Map[String, Int]] = {
    val map = mutable.Map[String, mutable.ListBuffer[(String, Int)]]()
    for (tftuple <- TfStream) {
      map.getOrElseUpdate(tftuple.term, mutable.ListBuffer[(String, Int)]())
      map(tftuple.term) += ((tftuple.doc, tftuple.count))
    }
    map
       .filter(_._2.size > 2) // choose terms appear in at least 3 docs
         .filter(_._2.size < 4000)
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
    val docs = new DocIndex(fname)

//    for (i <-  docs.fqIndex) {
//      println(i)
//    }
//
//    for (i <- docs.fwIndex) {
//      println(i)
//    }

//    println(docs.Pwd)

//    println(docs.lambdad)
//
//    println( docs.fqIndex.size)
    val dist = docs.fqIndex.mapValues(_.size).groupBy(_._2).mapValues(_.map(_._1))
    for (i <- dist.keySet.toList.sorted) {

      println(i, dist(i).size, dist(i))
    }
  }
}