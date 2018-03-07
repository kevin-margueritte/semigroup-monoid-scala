import kmargueritte.definition.Monoid
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

trait MonoidLaws[A] extends PropertyChecks with Matchers {

  def associativity(monoid: Monoid[A], gen: Gen[A]) = {
    val generator = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    forAll(generator) { case (x,y,z) =>
      monoid.combine(monoid.combine(x, y), z) should equal (monoid.combine(z, monoid.combine(x, y)))
    }
  }

  def identity(monoid: Monoid[A], gen: Gen[A]) = {
    val generator = for {
      x <- gen
    } yield x

    forAll(generator) { x =>
      monoid.combine(x, monoid.empty) == monoid.combine(monoid.empty, x)
    }
  }

}