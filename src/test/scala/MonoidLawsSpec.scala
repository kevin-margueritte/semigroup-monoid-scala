import kmargueritte.definition.Monoid
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters


trait MonoidLaws[A] {

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

  def identity(monoid: Monoid[A], gen: Gen[A]) = {
    val generator = for {
      x <- gen
    } yield x

    Prop.forAll(generator) { x =>
      monoid.combine(x, monoid.empty) == monoid.combine(monoid.empty, x)
    }
  }

}