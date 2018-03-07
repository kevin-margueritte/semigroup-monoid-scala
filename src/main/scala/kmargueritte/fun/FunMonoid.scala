package kmargueritte.fun

import kmargueritte.definition.Monoid

object FunMonoid {

  val intAdditionMonoid = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def empty: Int = 0
  }

  val intMultiplicaion = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x * y
    override def empty: Int = 0
  }

  val booleanOr = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def empty: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
    override def empty: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    override def empty: Option[A] = None
  }

  def endoMoinoid[A] = new Monoid[A => A] {
    override def combine(x: A => A, y: A => A): A => A = x.compose(y)
    override def empty: A => A = a => a
  }

  def foldMap[A,B](l: List[A], m: Monoid[B])(f: A => B): B = {
    l.foldLeft(m.empty)((x,y) => m.combine(x, f(y)))
  }

  //def foldRightViaFoldMap[A,B](l: List[A])(z: B)(f: (B, A) => B): B = {
    //foldMap(l)
  //}
}
