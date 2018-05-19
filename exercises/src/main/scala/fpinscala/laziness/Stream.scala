package fpinscala.laziness

import Stream._
trait Stream[+A] {

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

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = foldRight(Nil: List[A])(_ :: _)

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).take(5).toList2
  def take(n: Int): Stream[A] = {
    def go(i: Int, s: Stream[A]): Stream[A] = i match {
      case 0 => Empty
      case _ => s match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => go(i-1, t()))
      }
    }

    go(n, this)
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).drop(5).toList
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop(n-1)
    case _ => this
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).takeWhile(_ < 5).toList
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).forAll(_ > 0)
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if (!p(h())) => { println("false"); false }
    case Cons(_, t) => { println("Check tail"); t() forAll(p) }
    case _ => { println("true"); true }
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).takeWhile_foldRight(_ < 5).toList
  def takeWhile_foldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  // fpinscala.laziness.Stream(7,8,9).headOption
  // fpinscala.laziness.Stream().headOption
  def headOption: Option[A] = 
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).map(_ + 1)
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).filter(_ % 2 == 0)
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).append(Stream(10,11,12))
  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).flatmap(i => fpinscala.laziness.Stream(i,i,i))
  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).map_unfold(_ + 1).toList
  def map_unfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).take_unfold(5).toList
  def take_unfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 1))
      case (Cons(h, t), i) => Some(h(),(t(), i - 1))
      case _ => None
    }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).takeWhile_unfold(_ < 5).toList
  def takeWhile_unfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).zipWith_unfold(fpinscala.laziness.Stream(10, 20, 30, 40))(_ * _)
  def zipWith_unfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).zipAll_unfold(fpinscala.laziness.Stream(10, 20, 30, 40)).toList
  // fpinscala.laziness.Stream(1,2,3).zipAll_unfold(fpinscala.laziness.Stream(10, 20, 30, 40)).toList
  def zipAll_unfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case _ => None
    }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).startsWith(fpinscala.laziness.Stream(1, 2, 3, 4))
  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).startsWith(fpinscala.laziness.Stream(1, 2, 3, 5))
  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).startsWith(fpinscala.laziness.Stream(1, 2, 2, 4))
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll_unfold(s).takeWhile_unfold(t => t match {
      case (_, Some(_)) => true
      case _ => false
    }).forAll { case (h1, h2) => h1 == h2 }

  // fpinscala.laziness.Stream(1,2,3).tails.map(t => t.toList).toList
  def tails: Stream[Stream[A]] =
    unfold[Stream[A], Stream[A]](this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    } append Stream(empty)

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).hasSubsequence(fpinscala.laziness.Stream(3, 4, 5, 6))
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // fpinscala.laziness.Stream(1,2,3).scanRight(0)(_ + _).toList
  // fpinscala.laziness.Stream(1,2,3).scanRight(fpinscala.laziness.Stream.empty[Int])((a, b) => fpinscala.laziness.Stream.cons(a, b)).map(t => t.toList).toList
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((h, s) => {
      val (agg1, agg2) = s
      val newAgg1 = f(h, agg1)
      (newAgg1, cons(newAgg1, agg2))
    })._2
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

  // fpinscala.laziness.Stream.constant(5).take(6).toList
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  // fpinscala.laziness.Stream.from(6).take(5).toList
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // fpinscala.laziness.Stream.fibs().take(7).toList
  def fibs(): Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] =
      cons(i, go(j, i + j))

    go(0, 1)
  }

  // fpinscala.laziness.Stream.unfold(5)(x => Some((x, x+1))).take(6).toList
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(s: S): Stream[A] = f(s) match {
      case None => empty
      case Some((a, b)) => cons(a, go(b))
    }

    go(z)
  }

  // fpinscala.laziness.Stream.fibs_unfold().take(7).toList
  def fibs_unfold(): Stream[Int] =
    unfold((0, 1))({ case (a, b) => Some((a, (b, a + b))) })

  // fpinscala.laziness.Stream.from_unfold(1).take(7).toList
  def from_unfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  // fpinscala.laziness.Stream.constant_unfold(4).take(7).toList
  def constant_unfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  // fpinscala.laziness.Stream.ones_unfold.take(7).toList
  val ones_unfold: Stream[Int] =
    unfold(1)(_ => Some((1,1)))
}