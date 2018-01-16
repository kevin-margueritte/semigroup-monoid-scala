import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck._

class SemigroupOps extends FlatSpec with Matchers {

  "Semigroup" should "have a correct max operator" in {

    import kmargueritte.implementation.SemigroupOps.max
    import syntax._

    Prop.forAll { (x: Int, y: Int) =>
      (x |+| y) == (if (x < y) x else y)
    }.check

  }

  "Semigroup" should "have a correct min operator" in {

    import kmargueritte.implementation.SemigroupOps.min
    import syntax._

    Prop.forAll { (x: Int, y: Int) =>
      (x |+| y) == (if (x < y) y else x)
    }.check

  }

}
