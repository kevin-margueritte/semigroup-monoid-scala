import kmargueritte.definition.Monoid
import org.scalatest._
import org.scalacheck._
import prop._

class MonoidOps extends FlatSpec with Matchers with PropertyChecks with MonoidLaws[Int] {

  def twice(x: Int): Int = x * 2

  "Monoid sum" must
    "respect laws" in {
      import kmargueritte.implementation.MonoidOps.sum
      associativity(implicitly[Monoid[Int]], Gen.posNum[Int])
      identity(implicitly[Monoid[Int]], Gen.posNum[Int])
    }

    it must "have a correct sum operator" in {

      import kmargueritte.implementation.MonoidOps.sum
      import syntax._

      Prop.forAll { (x: Int, y: Int) =>
        (x |+| y) should be(x + y)
      }

      Prop.forAll { (x: Int, y: Int) =>
        (x.|+|(y)) == x + y
      }

      Prop.forAll { (x: Int, y: Int) =>
        (twice(x) |+| y) == x*2 + y
      }

      Prop.forAll { (l: List[Int]) =>
        sum.combineAll(l) == l.foldLeft(0)((x,y) => x + y)
      }

    }

  "Monoid mult" must
    "have a correct mult operator" in {

      import kmargueritte.implementation.MonoidOps.mult
      import syntax._

      Prop.forAll { (x: Int, y: Int) =>
        (x |+| y) == x * y
      }

      Prop.forAll { (l: List[Int]) =>
        mult.combineAll(l) == l.foldLeft(0)((x,y) => x * y)
      }

    }

    it must "respect laws" in {
      import kmargueritte.implementation.MonoidOps.mult
      associativity(implicitly[Monoid[Int]], Gen.posNum[Int])
      identity(implicitly[Monoid[Int]], Gen.posNum[Int])
    }

}
