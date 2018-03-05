import org.scalacheck.Gen
import org.scalacheck.Prop._

trait MonoidLaws[A] {

  def associativity(combine: (A, A) => A, gen: Gen[A]) = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)) { case (x,y,z) =>
      combine(combine(x, y), z) == combine(x, combine(x, y))
    }
  }

  def identity(combine: (A, A) => A, gen: Gen[A]) = {
    
  }

}