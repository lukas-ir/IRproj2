import ch.ethz.dal.tinyir.processing.{Document, StopWords, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer

/**
  * Created by lekkerbit on 11/30/16.
  */


class TfStream(fname: String){

  private case class TfTuple(term: String, doc: String, count: Int)

  private def tokenFilter(tokens: List[String]) : List[String] = {
    val minlength = 3
    val tokenlist = tokens.map(_.toLowerCase().replaceAll("[^a-zA-Z]+", ""))
    StopWords.filterOutSW(tokenlist.map(x => PorterStemmer.stem(x))).filter(_.length()>minlength).toList
  }

  private def tfTuples(docs: Stream[Document]) : Stream[TfTuple] = {
    var counter = 0
    docs.flatMap(d => tokenFilter(d.tokens)
                       .groupBy(identity)
                       .map{case(tk,lst) =>
                                TfTuple(tk, d.name, lst.length)})
  }

  private val tipster = new TipsterStream(fname)

  val fqIndex : Map[String, List[(String, Int)]] = {
    tfTuples(tipster.stream)
      .groupBy(_.term)
      .mapValues(_.map(tfT=>(tfT.doc, tfT.count)).sorted)
      .mapValues(_.toList)
  }
}

object test {
  def main(args: Array[String]): Unit = {
    val fname = "./data/documents"
    val docs = new TfStream(fname)
    val index = docs.fqIndex
    for (i <- index) {
      println(i)
    }
    print(index.size)
  }
}