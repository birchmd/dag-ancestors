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

  def random(maxWidth: Int, maxRank: Int): DagGenerator = {
    val gen = new DagGenerator()
    val rand = new scala.util.Random()

    val genesis = gen.genesis()

    (1 until maxRank).foldLeft(List(genesis)) {
      case (possibleParents, rank) =>
        val possibleHashes = possibleParents.map(_.id)
        val numNodes = rand.nextInt(maxWidth) + 1

        (1 to numNodes).toList.map { _ =>
          val numParents = rand.nextInt(numNodes / 2 + 1) + 1
          gen.nodeWithParents(
            NonEmptyList
              .fromListUnsafe(rand.shuffle(possibleHashes).take(numParents))
          )
        }
    }

    gen
  }
}
