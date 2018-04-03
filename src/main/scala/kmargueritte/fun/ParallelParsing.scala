package kmargueritte

import kmargueritte.definition.Monoid
import kmargueritte.fun.FunMonoid

object ParallelParsing {

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    override def empty: WC = Stub("")
    override def combine(x: WC, y: WC): WC = {
      (x, y) match {
        case (Stub(l), Stub(r)) => Stub(l + r)
        case (Stub(c), Part(l, w, r)) => Part(c+l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r+c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          val totalWords = (w1 + w2) + ((r1 + l2).isEmpty match {
            case true => 0
            case false => 1
          })
          Part(l1, totalWords, r2)
      }
    }
  }

  def count(s: String): Int = {
    def charToWC(c: Char): WC = {
      c.isWhitespace match {
        case true => Stub(c.toString)
        case false => Part("", 0, "")
      }
    }

    def wcToInt(s: String) = if (s.isEmpty) 0 else 1

    FunMonoid.foldMapV(s.toIndexedSeq, wcMonoid)(charToWC) match {
      case Stub(x) => wcToInt(x)
      case Part(l, w, r) => wcToInt(l) + w + wcToInt(r)
    }
  }

}
