import ch.ethz.dal.tinyir.processing.{StopWords, XMLDocument}
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer

import collection.mutable

case class TfTuple(term: String, doc : String, count: Int)

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

class DocIndex(fname: String){

  private val filename = fname

  def tkstream(in: TipsterStream) = {
    var count = 0
    in.stream.map(
      doc => {
        count += 1
        println(count)
        (doc.name, Tokenizer.tokenize(doc.content).groupBy(identity)
                                         .mapValues(_.length))
      }
    )
  }

  private def TfStream : Stream[TfTuple] = {
    var count = 0
    val in = new TipsterStream(filename)
    in.stream.flatMap{ doc =>
      {
        println(count)
        count += 1
        Tokenizer.tokenize(doc.content).groupBy(identity)
          .map(tkfq => TfTuple(tkfq._1.intern(), doc.name.intern(), tkfq._2.length))

      }
    }
  }



  /* token -> list of docID containing this token and its frequency */
  lazy val fqIndex : Map[String, List[(String, Int)]] = {
    val map = mutable.Map[String, mutable.ListBuffer[(String, Int)]]()
    for (tftuple <- TfStream) {
      map.getOrElseUpdate(tftuple.term, mutable.ListBuffer[(String, Int)]())
      map(tftuple.term) += ((tftuple.doc, tftuple.count))
    }
    map
       .filter(_._2.size > 1) // choose terms appear in at least two docs
       .mapValues(_.toList.sorted)
       .toMap
  }
}

object test {
  def main(args: Array[String]): Unit = {
    val fname = "./data/documents"
    val docs = new DocIndex(fname)

    for (i <-  docs.fqIndex) {
      println(i)
    }
    print( docs.fqIndex.size)
  }
}