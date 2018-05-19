package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // val rnd = fpinscala.state.RNG.Simple(12)
  // fpinscala.state.RNG.nonNegativeInt(rnd)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    (if (v < 0) v + 1 + Int.MaxValue else v, r)
  }

  // val rnd = fpinscala.state.RNG.Simple(12)
  // fpinscala.state.RNG.double(rnd)
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  // fpinscala.state.RNG.intDouble(rnd)
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  // fpinscala.state.RNG.doubleInt(rnd)
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  // fpinscala.state.RNG.double3(rnd)
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // fpinscala.state.RNG.ints(5)(rnd)
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil: List[Int], rng)
      case c => {
        val (i, rng2) = rng.nextInt
        val (l, rng3) = ints(c - 1)(rng2)
        (i :: l, rng3)
      }
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // fpinscala.state.RNG.double_rand(rnd)
  def double_rand: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  // fpinscala.state.RNG.randIntDouble(rnd)
  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  // fpinscala.state.RNG.randDoubleInt(rnd)
  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
