package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(i, h)

  // Hints:
  // x insert any 2 into help, then findMin will get the smaller of the two elements
  // x insert 1, then deleteMin, the heap should be empty
  // x any arbitrary heap should return sorted list of elements when repeatedly findMin and deleteMin
  // x findMin on melded heaps should return the min of one or the other.

  property("insert 2") = forAll { (i1: Int, i2: Int) =>
    val h = insert(i2, insert(i1, empty))

    findMin(h) == Math.min(i1, i2)
  }

  property("empty") = forAll { i: Int =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  def toList(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h) +: toList(deleteMin(h))
  def fromList(l: List[A]): H = l match {
    case Nil => empty
    case head :: rest => insert(head, fromList(rest))
  }

  property("sort2") = forAll { (a: List[Int]) =>
    val h= fromList(a)
    val l = toList(h)
    a.length == l.length &&
      a.sortWith((e1, e2) => e1 < e2).equals(l)
  }

  property("melded") = forAll { (h1: H, h2: H) =>
    val meldedH = meld(h1, h2)

    val meldedMin = findMin(meldedH)
    (meldedMin == findMin(h1)) || (meldedMin == findMin(h2))
  }

  property("compare melding") = forAll { (h1: H, h2: H) =>
    def equal(heap1:H, heap2:H):Boolean =
      if (isEmpty(heap1) && isEmpty(heap2)) true
      else {
        val m1 = findMin(heap1)
        val m2 = findMin(heap2)
        (m1 == m2) && equal(deleteMin(heap1), deleteMin(heap2))
      }

    val initialMeld = meld(h1, h2)
    val oppositeMeld = meld(h2, h1)

    equal(initialMeld, oppositeMeld)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
