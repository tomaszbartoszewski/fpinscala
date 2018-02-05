package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // datastructures.Tree.size(datastructures.Branch(datastructures.Branch(datastructures.Leaf(1), datastructures.Leaf(2)), datastructures.Leaf(3)))
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // datastructures.Tree.maximum(datastructures.Branch(datastructures.Branch(datastructures.Leaf(4), datastructures.Leaf(6)), datastructures.Leaf(3)))
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

}