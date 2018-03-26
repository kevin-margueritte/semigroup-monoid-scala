package kmargueritte.laws

import kmargueritte.definition.Monoid
import org.scalacheck.{Gen, Prop}

trait SemigroupLawSpec[A] {

  def associativity(monoid: Monoid[A], gen: Gen[A]) = {
    val generator = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    Prop.forAll(generator) { case (x,y,z) =>
      monoid.combine(monoid.combine(x, y), z) == monoid.combine(z, monoid.combine(x, y))
    }
  }

}
