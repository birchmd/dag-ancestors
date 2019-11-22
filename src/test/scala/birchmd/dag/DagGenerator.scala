package birchmd.dag

import cats.data.NonEmptyList
import DagGenerator.Hash
import Instances.SimpleNode
import scala.collection.mutable.HashMap

class DagGenerator {
  private val ids = Iterator.iterate(0L)(_ + 1L).buffered
  private val idMapping = HashMap.empty[Hash, SimpleNode]
  private val childrenMapping = HashMap.empty[Hash, List[Hash]]
  private val ancestorMapping = HashMap.empty[(Hash, Long), List[Hash]]

  def genesis(): SimpleNode =
    if (ids.head > 0) idMapping(0)
    else {
      val id = ids.next()
      val node = SimpleNode(id, Nil, 0L)
      idMapping.update(id, node)
      DagGenerator.powers.foreach { r =>
        ancestorMapping.update(id -> r, Nil)
      }
      node
    }

  def maxUsedIndex: Long = ids.head - 1L

  def nodeByHash(hash: Hash): SimpleNode = idMapping(hash)

  def nodeWithParents(ps: NonEmptyList[Hash]): SimpleNode = {
    val parents = ps.toList
    val id = ids.next()
    val parentRanks = parents.map(p => idMapping(p).rank)
    val rank = parentRanks.max + 1
    val node = SimpleNode(id, parents, rank)

    idMapping.update(id, node)

    parents.foreach { p =>
      val children = childrenMapping.getOrElse(p, Nil)
      childrenMapping.update(p, id :: children)
    }

    val _ = DagGenerator.powers.foldLeft(parents) {
      case (ancestors, r) =>
        ancestorMapping.update(id -> r, ancestors)

        ancestors.flatMap(a => ancestorMapping(a -> r))
    }

    node
  }

  def naiveDag[F[_]](
      implicit monadError: cats.MonadError[F, Throwable]
  ): NaiveDag[F, SimpleNode, Hash] = {
    val nodeImpl = Instances.NodeImpl
    val hashesStorage = Instances.InMem[F, Hash, SimpleNode](idMapping.toMap)
    val childrenStorage =
      Instances.InMem[F, Hash, List[Hash]](childrenMapping.toMap)
    new NaiveDag(nodeImpl, hashesStorage, childrenStorage)
  }

  def skipDag[F[_]](
      implicit monadError: cats.MonadError[F, Throwable]
  ): SkipDag[F, SimpleNode, Hash] = {
    val nodeImpl = Instances.NodeImpl
    val hashesStorage = Instances.InMem[F, Hash, SimpleNode](idMapping.toMap)
    val childrenStorage =
      Instances.InMem[F, Hash, List[Hash]](childrenMapping.toMap)
    val ancestorStorage =
      Instances.InMem[F, (Hash, Long), List[Hash]](ancestorMapping.toMap)
    new SkipDag(nodeImpl, hashesStorage, childrenStorage, ancestorStorage)
  }
}

object DagGenerator {
  type Hash = Long
  val powers = List(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L, 512L, 1024L)
}
