package fpinscala.datastructures

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
	def size[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 0
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A, B](t: Tree[A])(f: Leaf[A] => B)(g: (B, B) => B): B = t match {
		case Leaf(v) => f(Leaf(v))
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def foldDepth[A](t: Tree[A]): Int = fold(t)(_ => 0)((a: Int, b: Int) => 1 + (a max b))
	def foldMaximum(t: Tree[Int]): Int = fold(t) ((l: Leaf[Int]) => l.value) ((a: Int, b: Int) => a max b)
	def foldSize[A](t: Tree[A]): Int = fold(t) ((l: Leaf[A]) => 1) ((a: Int, b: Int) => 1 + a + b)

	def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
		fold(t) ((l: Leaf[A]) => {Leaf(f(l.value))} : Tree[B]) ((l: Tree[B], r: Tree[B]) => Branch(l, r))
	}
}
