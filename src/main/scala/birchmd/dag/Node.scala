package birchmd.dag

trait Node[A, Hash] {
  def id(a: A): Hash
  def parents(a: A): List[Hash]
  def rank(a: A): Long
}
