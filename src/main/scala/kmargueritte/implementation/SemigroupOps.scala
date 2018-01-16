package kmargueritte.implementation

import kmargueritte.definition.Semigroup

object SemigroupOps {

  implicit val max = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = if (x < y) x else y
  }

  implicit val min = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = if (x < y) y else y
  }

}
