import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument

import scala.collection.mutable
import scala.math._

/** TipsterSteam partition
  *
  * @param parentStream   stream to partition
  * @param partitionIds   ids of documents (enumeration starts at 0 and ends at parentStream.length) belonging to the partition
  */
case class TipsterStreamPartition(val parentStream: TipsterStream, val partitionIds : Set[Int]) {
  def stream: Stream[XMLDocument] = parentStream.stream.zipWithIndex
                                      .filter { case (doc, id) => partitionIds(id) }.map(_._1)
  def length: Int = partitionIds.size
}

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

  /** Create a random fractional partition of a TipsterStream document collection
    *
    * @param parentStream   stream to partition
    * @param fraction       fraction size
    * @return               partitioned stream
    */
  def create(parentStream: TipsterStream, fraction : Double): TipsterStreamPartition = {
    return new TipsterStreamPartition(parentStream,generatePartitionIds(parentStream.length, fraction))
  }
}