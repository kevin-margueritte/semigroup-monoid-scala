import kmargueritte.definition.Semigroup

package object syntax {

  implicit class SemigroupOps[A](x: A)(implicit semigroup: Semigroup[A]) {
    def |+|(y: A): A = semigroup.combine(x,y)
  }

}
