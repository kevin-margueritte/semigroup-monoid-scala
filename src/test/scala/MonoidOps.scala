import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck._

class MonoidOps extends FlatSpec with Matchers {

  def twice(x: Int): Int = x * 2

  "IntMonoid" should "have a correct sum operator" in {

    import kmargueritte.implementation.MonoidOps.sum
    import syntax._

    Prop.forAll { (x: Int, y: Int) =>
      (x |+| y) == x + y
    }.check

    Prop.forAll { (x: Int, y: Int) =>
      (x.|+|(y)) == x + y
    }.check

    Prop.forAll { (x: Int, y: Int) =>
      (twice(x) |+| y) == x*2 + y
    }.check

    Prop.forAll { (l: List[Int]) =>
      sum.combineAll(l) == l.foldLeft(0)((x,y) => x + y)
    }.check

  }

  "IntMonoid" should "have a correct mult operator" in {

    import kmargueritte.implementation.MonoidOps.mult
    import syntax._

    Prop.forAll { (x: Int, y: Int) =>
      (x |+| y) == x * y
    }.check

    Prop.forAll { (l: List[Int]) =>
      mult.combineAll(l) == l.foldLeft(0)((x,y) => x * y)
    }.check

  }

}
