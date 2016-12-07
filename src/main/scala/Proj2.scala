import java.io.{BufferedInputStream, FileInputStream, InputStream}

import ch.ethz.dal.tinyir.processing.{TipsterParse, XMLDocument}


class QueryParse(is: InputStream) extends XMLDocument(is) {
  override def title  : String = ""
  override def body   : String = read(doc.getElementsByTagName("TEXT"))
  override def name   : String = read(doc.getElementsByTagName("DOCNO")).filter(_.isLetterOrDigit)
  override def date   : String = ""
  override def content: String = body
  def top: String = read(doc.getElementsByTagName("top"))
}

object Proj2 {
  def main(args: Array[String]): Unit = {
    val path = "./data/documents"
    val qureypath = "./data/questions-descriptions.txt"

//    val unparsed = new BufferedInputStream(new FileInputStream(qureypath))
//    val query = new QueryParse(unparsed)

//    println(query.top)

    val index = new DocIndex(path)
    val lm = new LanguageModel(index)
    lm.predict("Airbus Subsidies")



  }
}
