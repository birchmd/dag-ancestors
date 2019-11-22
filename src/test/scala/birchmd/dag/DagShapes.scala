package birchmd.dag

import cats.data.NonEmptyList

object DagShapes {
  def chain(length: Int): DagGenerator = {
    val gen = new DagGenerator()
    val genesis = gen.genesis()

    val _ = (1 until length).foldLeft(genesis) {
      case (block, _) => gen.nodeWithParents(NonEmptyList.of(block.id))
    }

    gen
  }

  def fork(length: Int): DagGenerator = {
    val gen = new DagGenerator()

    val genesis = gen.genesis()
    val a = gen.nodeWithParents(NonEmptyList.of(genesis.id))
    val b = gen.nodeWithParents(NonEmptyList.of(genesis.id))

    val _ = (2 until length).foldLeft(a -> b) {
      case ((left, right), _) =>
        (
          gen.nodeWithParents(NonEmptyList.of(left.id)),
          gen.nodeWithParents(NonEmptyList.of(right.id))
        )
    }

    gen
  }
}
