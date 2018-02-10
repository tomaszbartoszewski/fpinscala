package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).map(_ * 2)
  // fpinscala.errorhandling.Some(5).map(_ * 2)
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).getOrElse(-1)
  // fpinscala.errorhandling.Some(5).getOrElse(-1)
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).flatMap(a => fpinscala.errorhandling.Some(15))
  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).flatMap(a => fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int])
  // fpinscala.errorhandling.Some(5).flatMap(a => fpinscala.errorhandling.Some(15))
  // fpinscala.errorhandling.Some(5).flatMap(a => fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int])
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse(None)

  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).orElse(fpinscala.errorhandling.Some(15))
  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).orElse(fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int])
  // fpinscala.errorhandling.Some(5).orElse(fpinscala.errorhandling.Some(15))
  // fpinscala.errorhandling.Some(5).orElse(fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int])
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse(ob)

  // (fpinscala.errorhandling.None: fpinscala.errorhandling.Option[Int]).filter(a => a == 5)
  // fpinscala.errorhandling.Some(5).filter(a => a == 5)
  // fpinscala.errorhandling.Some(10).filter(a => a == 5)
  def filter(f: A => Boolean): Option[A] =
    map (a => if (f(a)) Some(a) else None) getOrElse(None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // fpinscala.errorhandling.Option.variance(Seq())
  // fpinscala.errorhandling.Option.variance(Seq(1,2,3,4,5))
  // fpinscala.errorhandling.Option.variance(Seq(2,2,2,2))
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  // fpinscala.errorhandling.Option.abs0(fpinscala.errorhandling.Some(-12.98))
  // fpinscala.errorhandling.Option.abs0(fpinscala.errorhandling.None)
  val abs0: Option[Double] => Option[Double] =
    lift(math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    math.abs(45 - age) + numberOfSpeedingTickets

  // fpinscala.errorhandling.Option.parseInsuranceRateQuote("", "")
  // fpinscala.errorhandling.Option.parseInsuranceRateQuote("25", "12")
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(av), Some(bv)) => Some(f(av, bv))
  }


  // import fpinscala.errorhandling._
  // Option.sequence(List(Some(5), Some(4), Some(9)))
  // Option.sequence(List(Some(5), Some(4), Some(9), None, Some(8)))
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(h, sequence(t))(_ :: _)
    }

  }

  // Option.traverse(List("2", "3", "4", "5"))(i => Option.Try(i.toInt))
  // Option.traverse(List("2", "3", "4", "5", "wrong"))(i => Option.Try(i.toInt))
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  // Option.sequence_traverse(List(Some(5), Some(4), Some(9), None, Some(8)))
  // Option.sequence_traverse(List(Some(5), Some(4), Some(9), Some(8)))
  def sequence_traverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  // Option.map2_for(Some(5), Some(6))(_ * _)
  // Option.map2_for(Some(5), None: Option[Int])(_ * _)
  // Option.map2_for(None: Option[Int], Some(3))(_ * _)
  def map2_for[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
}