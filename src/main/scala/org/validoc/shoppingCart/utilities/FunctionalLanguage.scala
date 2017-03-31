package org.validoc.shoppingCart.utilities

// Money, Rewards, Offers and ShoppingCarts are naturally Monoids. By making this a type class
//I can control the usage of arithmetic on them, and make it really easy to change their representation
//In money and offers case that is very likely!
trait Monoid[T] {
  def zero: T

  def add(t: T, other: T): T

  def timesBy(t: T, num: Int): T = List.fill(num)(t).foldLeft(zero)(add)
}


trait Group[T] extends Monoid[T] {
  def inverse(t: T) = t

  def subtract(t: T, other: T): T = add(t, inverse(other))
}

trait Functor[M[_]] {
  def fmap[A, B](m: M[A], fn: A => B): M[B]
}

trait Sequence[M[_]] extends Functor[M] {
  def +[T](m: M[T], t: T): M[T]
}

trait Monad[M[_]] extends Functor[M] {
  def lift[T](t: T): M[T]

  def flatMap[A, B](m: M[A], fn: A => M[B]): M[B]
}


object FunctionalLanguage {

  implicit object SeqAsMonadAndSequence extends Monad[Seq] with Sequence[Seq] {
    override def lift[T](t: T): Seq[T] = Seq(t)

    override def flatMap[A, B](m: Seq[A], fn: (A) => Seq[B]): Seq[B] = m.flatMap(fn)

    override def +[T](m: Seq[T], t: T): Seq[T] = m ++ Seq(t)

    override def fmap[A, B](m: Seq[A], fn: (A) => B): Seq[B] = m.map(fn)
  }

  implicit def SeqAsMonoid[T] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq()

    override def add(t: Seq[T], other: Seq[T]): Seq[T] = t ++ other
  }


  implicit class MonoidPimper[T: Monoid](t: T) {
    val monoid = implicitly[Monoid[T]]

    def +(other: T) = monoid.add(t, other)

    def *(n: Int) = monoid.timesBy(t, n)
  }

  implicit class GroupPimper[T: Group](t: T) {
    val group = implicitly[Group[T]]

    def negate = group.inverse(t)

    def -(other: T) = group.subtract(t, other)
  }

  implicit class MonoidSeqPimper[T: Monoid](ts: Seq[T]) {
    val monoid = implicitly[Monoid[T]]

    import monoid._

    def addUp = ts.foldLeft(zero)(add)
  }

  implicit class FunctorPimper[M[_] : Functor, A](f: M[A]) {
    def fmap[B](fn: A => B): M[B] = implicitly[Functor[M]].fmap(f, fn)
  }

  implicit class SequencePimper[M[_] : Sequence, T](s: M[T]) {
    def +(t: T): M[T] = implicitly[Sequence[M]].+(s, t)
  }

  implicit class MonadPimper[M[_] : Monad, A](m: M[A]) {
    def flatMap[B](fn: A => M[B]) = implicitly[Monad[M]].flatMap(m, fn)
  }

}
