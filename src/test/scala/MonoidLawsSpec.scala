import kmargueritte.definition.Monoid
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._

trait MonoidLaws[A] {

  def associativity(monoid: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)) { case (x,y,z) =>
      monoid.combine(monoid.combine(x, y), z) == monoid.combine(x, monoid.combine(x, y))
    }
  }

  def identity(monoid: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
    } yield x) { x =>
      monoid.combine(x, monoid.empty) == monoid.combine(monoid.empty, x)
    }
  }

}