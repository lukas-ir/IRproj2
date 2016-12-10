import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer


/** Preprocesses a document content into tokens eliminating stop-words and using PorterStemmer
  *
  * FIXME: Why are we replacing numeric values? And in-word punctuation?
  */
object Tokenizer {
  def tokenize(content: String) = {
    val tokens = content.toLowerCase()
      .replaceAll("[^a-z ]", " ")
      .split(" ")
      .filter(_.length >= 3)

    StopWords.filterOutSW(tokens)
      .map(PorterStemmer.stem)

  }
}