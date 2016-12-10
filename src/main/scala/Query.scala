import scala.io.Source

/** Query-related functionality
  */
object Query {
  /** Load queries from file
    *
    * @param path  query file path (e.g. "./data/questions-descriptions.txt")
    * @return      map from query ID to query string
    */
  def load(path: String) =
  Source.fromFile(path).getLines()
    .filter(_.matches(".*<num>.*|.*<title>.*"))
    .map(_.split("[ ]+", 3)
      .last/*.slice(2,3)(0).mkString("")*/.trim())
    .grouped(2).map(q=>(q(0).toInt, q(1)))
    .toMap
}