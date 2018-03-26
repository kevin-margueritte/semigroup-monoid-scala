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

  def endoMonoid[A] = new Monoid[A => A] {
    override def combine(x: A => A, y: A => A): A => A = x.compose(y)
    override def empty: A => A = a => a
  }

  def reverse[A](monoid: Monoid[A]) = new Monoid[A] {
    override def combine(x: A, y: A): A = monoid.combine(y,x)
    override def empty: A = monoid.empty
  }

  def foldMap[A,B](l: List[A], m: Monoid[B])(f: A => B): B = {
    l.foldLeft(m.empty)((x,y) => m.combine(x, f(y)))
  }

  // (A, B => B) => B
  def foldRightViaFoldMap[A,B](l: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap[A, B => B](l, endoMonoid[B])(f.curried).apply(z)
  }

  // (B => B, A) => B
  def foldLeftViaFoldMap[A,B](l: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap[A, B => B](l, reverse(endoMonoid[B]))(a => b => f(b, a)).apply(z)
  }

  def ordered(v: IndexedSeq[Int]): Boolean= {
    def monoidOrder = new Monoid[Option[Int]] {
      override def combine(x: Option[Int], y: Option[Int]): Option[Int] = {
        (x, y) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(x), Some(y)) =>
            if (x < y) Some(y)
            else None
        }
      }
      override def empty: Option[Int] = Some(Int.MinValue)
    }

    foldMapV(v, monoidOrder)(Some.apply) match {
      case Some(_) => true
      case None => false
    }
  }

  def foldMapV[A, B](l: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    l.size match {
      case 0 => m.empty
      case 1 => f(l.head)
      case x => {
        val (first, second) = l.splitAt(x/2)
        m.combine(foldMapV(first, m)(f), foldMapV(second, m)(f))
      }
    }
  }
}
