
import ch.ethz.dal.tinyir.io.ZipStream
import ch.ethz.dal.tinyir.io.TipsterStream

object Proj2 {
  def main(args: Array[String]): Unit = {
    val path = "./data/documents"
    val docs = new TipsterStream(path)
    println(docs.length)

    println("Number of files in zips = " + docs.length)

    var length: Long = 0
    var tokens: Long = 0
    for (doc <- docs.stream.take(10000)) {
      length += doc.content.length
      tokens += doc.tokens.length
    }
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }
}
