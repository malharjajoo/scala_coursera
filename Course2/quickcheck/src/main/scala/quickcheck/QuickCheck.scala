package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  // Gen.oneOf chooses from one of the generators provided to it.
  // If constant values are given, it implicitly creates a generator
  // that generates only that value.
  lazy val genHeap: Gen[H] = Gen.oneOf(
    const(empty),
    for{
      mynodeval <- arbitrary[Int]
      myheap <- Gen.oneOf(const(empty), genHeap)
    } yield insert(mynodeval, myheap)
  )

  lazy val genNonEmptyHeap: Gen[H] = for{
      mynodeval <- arbitrary[Int]
      myheap <- Gen.oneOf(const(empty), genNonEmptyHeap)
    } yield insert(mynodeval, myheap)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insert_2elems_Into_EmptyHeap") = Prop.forAll(const(empty)) { (h: H) =>
    val m = 10
    val n = 100
    val new_heap = insert(n, h)
    val new_heap_2 = insert(m, new_heap)

    // inserted value should be the minimum since input heap is empty
    val b1 = findMin(new_heap) == n

    // Minimum value of the two inserted values is returned.
    val b2 = findMin(new_heap_2) == m

    // Ensure the other value is not lost ...
    val b3 = findMin(deleteMin(new_heap_2)) == n

    b1 && b2 && b3
  }

  property("insert_delete_elem_Into_EmptyHeap") = Prop.forAll(const(empty)) { (h: H) =>
    val m = 10
    isEmpty(deleteMin( insert(m, h)))
  }

  property("check_delete_elem_Into_EmptyHeap") = Prop.forAll(const(empty)) { (h: H) =>
    val m = 10
    val n = 20
    val p = 200

    val new_heap = insert(p, insert(n, insert(m, h)))
    val new_heap_2 = deleteMin(new_heap) // p and n should now remain

    findMin(new_heap_2) != m
  }


  def checkSorted(h: H): Boolean = {

    def helper(heap: H): List[A] =
    {
      if(isEmpty(heap)) {
        List[A]()
      }
      else
      {
        findMin(heap)::helper(deleteMin(heap))
      }
    }

      val x = helper(h)
      x == x.sorted[A](ord)
  }

  property("fundamental_prop_NonEmpty") = Prop.forAll{ (h: H) =>
    if(isEmpty(h)) true
    else checkSorted(h)
  }

  property("min_of_meld_prop_NonEmpty") = Prop.forAll(genNonEmptyHeap, genNonEmptyHeap){ (h1: H, h2: H) =>

    if(isEmpty(h1)){}
    val x = findMin(h1)
    val y = findMin(h2)
    val z = findMin( meld(h1,h2))

    if (x <= y) z == x
    else z == y
  }

}
