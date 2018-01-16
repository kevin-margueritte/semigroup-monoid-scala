package kmargueritte.definition

trait Monoid[A] extends Semigroup[A] {
  def empty: A
  def combineAll(l: List[A]): A = l.foldLeft(empty)((x,y) => combine(x,y))
}
