import kmargueritte.definition.Monoid
import kmargueritte.laws.MonoidLaws
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers._

class MonoidOps extends FlatSpec with MonoidLaws[Int] with PropertyChecks with Matchers {

  def twice(x: Int): Int = x * 2

  "Monoid sum" should
    "respect laws" in {
      import kmargueritte.implementation.MonoidOps.sum
      check(associativity(implicitly[Monoid[Int]], Gen.posNum[Int]))
      check(identity(implicitly[Monoid[Int]], Gen.posNum[Int]))
    }

    it must "have a correct sum operator" in {

      import kmargueritte.implementation.MonoidOps.sum
      import syntax._

      forAll { (x: Int, y: Int) =>
        (x |+| y) should equal(x + y)
      }

      forAll { (x: Int, y: Int) =>
        (x.|+|(y)) should equal(x + y)
      }

      forAll { (x: Int, y: Int) =>
        (twice(x) |+| y) should equal(x*2 + y)
      }

      forAll { (l: List[Int]) =>
        sum.combineAll(l) should equal(l.foldLeft(0)((x,y) => x + y))
      }

    }

  "Monoid mult" should
    "have a correct mult operator" in {

      import kmargueritte.implementation.MonoidOps.mult
      import syntax._

      forAll { (x: Int, y: Int) =>
        (x |+| y) == x * y
      }

      forAll { (l: List[Int]) =>
        mult.combineAll(l) should equal (l.foldLeft(1)((x,y) => x * y))
      }

    }

    it must "respect laws" in {
      import kmargueritte.implementation.MonoidOps.mult
      check(associativity(implicitly[Monoid[Int]], Gen.posNum[Int]))
      check(identity(implicitly[Monoid[Int]], Gen.posNum[Int]))
    }

}
