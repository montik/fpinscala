package fpinscala.laziness

import Stream._
trait Stream[+A] {

	def toList: List[A] = this match {
		case Empty => Nil
		case Cons(x, xs) => x() :: xs().toList
	}

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  @annotation.tailrec
	final def drop(n: Int): Stream[A] = (this, n) match {
		case (Empty, _) => this
		case (_, 0) => this
		case (Cons(h, t), i) => t().drop(i-1)
	}

	/*
  def take(n: Int): Stream[A] = (this, n) match {
		case (Empty, _) => this
		case (_, 0) => Empty
		case (Cons(h, t), i) => cons(h(), t().take(n - 1)) 
	}
	*/

	def tail: Option[Stream[A]] = this match {
		case Empty => None
		case Cons(v, tail) => Some(tail())
	}

	/*
	def takeWhile(p: A => Boolean): Stream[A] =
		foldRight(Empty: Stream[A])((a: A, b) => if (p(a)) Cons(() => a, () => b) else Empty)
	*/

	def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

	def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
	//def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

	def filter(p: A => Boolean): Stream[A] =
		foldRight(Empty: Stream[A])((a: A, b) => if (p(a)) Cons(() => a, () => b) else b)

	def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

	def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s) forAll (x => x._1 == x._2)

	def map[B](f: A => B): Stream[B] = unfold(this)(s => s match {
		case Empty => None
		case Cons(v, tail) => Some(f(v()), tail())
	})

	def take(n: Int): Stream[A] = unfold((this, n))(status => status match {
		case (_, 0) => None
		case (Cons(h, t), n) => Some( (h(), (t(), n - 1)))
		case (Empty, _) => None
	})

	def takeWhile(p: A => Boolean) = unfold(this)(s => s match {
		case Empty => None
		case Cons(h, t) => if (p(h())) Some(h(), t()) else None
	})

	def zipWith[B, C](that: Stream[B])(f: (A, B) => C) =
		unfold(this, that)(status => {
			val (thi, tha) = status
			thi.headOption flatMap (thiHead => 
				tha.headOption map (thaHead => 
						(f(thiHead, thaHead), (thi.tail getOrElse(Empty), tha.tail getOrElse(Empty)))
				)
			)
		})

	def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
		unfold(this, s2)((streams) => streams match {
			case (Empty, Empty) => None
			case (a, b) => Some((a.headOption, b.headOption),
													(a.tail getOrElse(Empty), b.tail getOrElse(Empty)))
		})

	def tails: Stream[Stream[A]] = unfold(this)(s => s match { 
		case Empty => 

	//def hasSubsequence(s2: Stream[A]): Boolean = unfold((this, false))(s => 
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

		/*
	@annotation.tailrec
  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
	*/

 def from(n: Int): Stream[Int] = unfold(n)(x => Some(x + 1, x + 1))
 def constant[A](a: A): Stream[A] = unfold(Empty)(x => Some((a, Empty)))
 //def ones: Stream[Int] = unfold(Empty)(x => Some((1, Empty)))
 val ones: Stream[Int] = constant(1)

 def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
   f(z) map (x => cons(x._1, unfold(x._2)(f))) getOrElse Empty 

	//def fibs = cons(0, cons(1, fib(1, 1)))
	def fib(a: Int, b: Int): Stream[Int] = cons(b, fib(b, a + b))

	def fibs = unfold((0, 1))(x => {
		val (a, b) = x
		Some((a, (b, a + b)))
	})
}
