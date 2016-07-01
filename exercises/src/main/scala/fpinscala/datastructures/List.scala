package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

	/*
  @Override
  def append[A](a1: List[A], a2: List[A]): List[A] = 
		foldRight (a1, a2) ( Cons(_, _) )
	*/

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

	def concat[A](lss: List[List[A]]): List[A] = foldRight(lss, List[A]())(
		(l1: List[A], l2: List[A]) => append(l1, l2))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

	def sum1(ls: List[Int]): List[Int] = ls match {
		case Cons(x, xs) => Cons((x + 1), sum1(xs))
		case Nil => Nil
	}

	def doubleToString(ls: List[Double]): List[String] = map(ls)(_.toString)

	/* 3.22 */
	def zipSum(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
		case (_, Nil) => Nil
		case (Nil, _) => Nil
		case (Cons(l1s, l1ss), Cons(l2s, l2ss)) => Cons(l1s + l2s, zipSum(l1ss, l2ss))
	}

	def zipWith[A, B, C](l: List[A], m: List[B])(f: (A, B) => C): List[C] = (l, m) match {
		case (_, Nil) => Nil
		case (Nil, _) => Nil
		case (Cons(ls, lss), Cons(ms, mss)) => Cons(f(ls, ms), zipWith(lss, mss)(f))
	}

	def reverse[A](l: List[A]) = foldRight(l, Nil: List[A])((x: A, y: List[A]) => append(Cons(x, Nil), y))

	def isInitialSub[A](sup: List[A], sub: List[A]): Boolean = {
		val equalityList = zipWith(sup, sub)(_ == _)
		foldRight(equalityList, true)(_ && _)
	}

	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		if (length(sub) > length(sup)) false
		else sup match {
			case Nil => true
			case Cons(x, xs) => isInitialSub(sup, sub) || hasSubsequence(xs, sub)
		}
	}

  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

	def length[A](l: List[A]): Int = foldRight(l, 0)((_, e: Int) => 1 + e)

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(f(x), map(xs)(f))
	}
	
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
		case Nil => List[B]()
		case Cons(x, xs) => append(f(x), flatMap(xs)(f))
	}

	def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
	}

	def flatFilter[A](l: List[A])(f: A => Boolean): List[A] = 
		flatMap(l)((e: A) => if(f(e)) List(e) else List())

  def main(args: Array[String]): Unit = {
	}
}
