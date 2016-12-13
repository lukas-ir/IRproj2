import ch.ethz.dal.tinyir.io.{ParsedXMLStream, TipsterStream, ZipDirStream}
import ch.ethz.dal.tinyir.processing.{TipsterParse, XMLDocument}

import scala.collection.mutable
import scala.math._


/** TipsterSteam subsample (for rapid prototyping)
  *
  * @param path           path to TipsterStream data set
  * @param fraction       fraction of documents of base collection to serve
  */

class TipsterStreamSubsample (path: String, val fraction : Double, ext: String = "")
  extends ParsedXMLStream(new ZipDirStream(path, "")){
  def stream : Stream[XMLDocument] = unparsed.stream.zipWithIndex
    .filter { case (doc, id) => partitionIds(id) }.map(_._1).map(is => new TipsterParse(is))
  def length = unparsed.length

  /** Random enumeration document stream Ids
    */
  private val partitionIds : Set[Int] = {
    val maxId = unparsed.length
    val partitionSize = ceil(fraction * maxId)
    val resultPartition = new mutable.HashSet[Int]()
    while (resultPartition.size < partitionSize) {
      resultPartition += util.Random.nextInt(maxId)
    }
    resultPartition.toSet
  }
}


object TipsterStreamSubsample {

  /** Create a random fractional partition of a TipsterStream document collection
    *
    * @param path           path to Tipster data set
    * @param fraction       fraction size
    * @return               partitioned stream
    */
  def create(path: String, fraction : Double): TipsterStreamSubsample = {
    return new TipsterStreamSubsample(path,fraction)
  }
}