package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
// Это свойство проверяет, что при вставке элемента x в пустую кучу, он становится её минимумом, 
// а при последовательном добавлении элементов наименьшее значение остаётся минимальным.
  property("gen2") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }
// Это свойство проверяет, что объединение кучи с пустой кучей не меняет структуру и минимум первой кучи.
  property("gen3") = forAll { (h: H) =>
    meld(h, empty) == h && meld(empty, h) == h
  }
// Это свойство проверяет, что после нескольких операций вставки и удаления куча остаётся корректной 
// (например, не становится пустой, если не должна быть, или имеет корректное значение findMin).
  property("gen4") = forAll { (a: A, b: A, c: A) =>
    val h = insert(c, insert(b, insert(a, empty)))
    val h1 = deleteMin(h)
    val h2 = deleteMin(h1)
    !isEmpty(h2) && findMin(h2) == List(a, b, c).sorted.last
  }
