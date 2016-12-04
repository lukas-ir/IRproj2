import ch.ethz.dal.tinyir.processing.{StopWords}
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer


class DocIndex(fname: String){

  private case class TfTuple(term: String, doc: String, count: Int)

  /* Tokenize one document's content */
  private def tokenizer(content: String) : List[String] = {
    val minlength = 3
    val tokens = content.toLowerCase()
                        .replaceAll("[^a-z ]", " ")
                        .split(" ")
                        .filter(_.length()>minlength)

    StopWords.filterOutSW(tokens)
             .map(PorterStemmer.stem(_))
             .toList
  }

  private val tipster = new TipsterStream(fname)

  /* docID -> list of tokens and freqencies */
  val fwIndex: Map[String, Map[String, Int]] =
    tipster.stream.take(30000)
           .map{
//             var count = 0
             doc => {
//               count += 1
//               println(count)
               (doc.name, tokenizer(doc.content))
             }
           }
           .toMap
           .mapValues{_.groupBy(identity).mapValues(_.length)}


  /* token -> list of docID containing this token and its frequency */
  val fqIndex : Map[String, List[(String, Int)]] = {
//    var count = 0
    fwIndex
      .toList
      .flatMap{
        case (docid, tksfeq) => {
//          count += 1
//          println(count)
          tksfeq.map(tkfq => (tkfq._1, tkfq._2, docid))
        }
      }
      .groupBy(_._1)
      .mapValues(_.map(d => (d._3, d._2)))
  }

}

object test {
  def main(args: Array[String]): Unit = {
    val fname = "./data/documents"
    val docs = new DocIndex(fname)

    val fwindex = docs.fqIndex
    for (i <- fwindex) {
      println(i)
    }
    print(fwindex.size)
  }
}