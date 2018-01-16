package kmargueritte.implementation

import kmargueritte.definition.Monoid

object MonoidOps {

  implicit val sum = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val mult = new Monoid[Int] {
    override def empty: Int = 1
    override def combine(x: Int, y: Int): Int = x * y
  }

}
