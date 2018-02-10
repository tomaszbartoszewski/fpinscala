package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  // (Right[Int](15): Either[String, Int]).map(_ * 2)
  // (Left[String]("Error"): Either[String, Int]).map(_ * 2)
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(r) => Right(f(r))
  }

  // (Right[Int](15): Either[String, Int]).flatMap(x => Left[String]("Test"))
  // (Right[Int](15): Either[String, Int]).flatMap(x => Right[Int](x * 2))
  // (Left[String]("Test"): Either[String, Int]).flatMap(x => Right[Int](x * 2))
  // (Left[String]("Test"): Either[String, Int]).flatMap(x => Left[String]("Other"))
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(r) => f(r)
  }

  // (Right(15)).orElse(Right(30))
  // (Left("Error")).orElse(Right(30))
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(r) => Right(r)
  }

  // Right(15).map2(Right(4))(_ * _)
  // Right(15).map2(Left("Error"): Either[String, Int])(_ * _)
  // (Left("Test"): Either[String, Int]).map2(Left("Error"): Either[String, Int])(_ * _)
  // (Left("Test"): Either[String, Int]).map2(Right(15): Either[String, Int])(_ * _)
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap(a => b map (bb => f(a, bb)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}