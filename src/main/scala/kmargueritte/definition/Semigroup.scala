package kmargueritte.definition

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
