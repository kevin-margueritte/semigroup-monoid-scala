package kmargueritte.laws

import kmargueritte.definition.Monoid
import org.scalacheck.{Gen, Prop}


trait MonoidLaws[A] extends SemigroupLawSpec[A] {

  def identity(monoid: Monoid[A], gen: Gen[A]) = {
    val generator = for {
      x <- gen
    } yield x

    Prop.forAll(generator) { x =>
      monoid.combine(x, monoid.empty) == monoid.combine(monoid.empty, x)
    }
  }

}