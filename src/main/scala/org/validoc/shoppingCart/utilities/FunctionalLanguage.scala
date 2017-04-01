package org.validoc.shoppingCart.utilities

// Money, Rewards, Offers and ShoppingCarts are naturally Monoids. By making this a type class
//I can control the usage of arithmetic on them, and make it really easy to change their representation
//In money and offers case that is very likely!
trait Monoid[T] {
  def zero: T

  def add(t: T, other: T): T

  def timesBy(t: T, num: Int): T = List.fill(num)(t).foldLeft(zero)(add)
}


object Monoid {

  implicit object MonoidForInt extends Monoid[Int] {
    override def zero: Int = 0

    override def add(t: Int, other: Int): Int = t + other
  }

  implicit def seqAsMonoid[T] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq()

    override def add(t: Seq[T], other: Seq[T]): Seq[T] = t ++ other
  }


}

class MonoidFor[M, T: Monoid](fn: T => M, coFn: M => T) extends Monoid[M] {
  val monoidT = implicitly[Monoid[T]]

  override def zero: M = fn(monoidT.zero)

  override def add(t: M, other: M): M = fn(monoidT.add(coFn(t), coFn(other)))

}

trait Group[T] extends Monoid[T] {
  def inverse(t: T): T

  def subtract(t: T, other: T): T = add(t, inverse(other))

}

object Group {

  implicit object GroupForInt extends Group[Int] {
    override def inverse(t: Int): Int = -t

    override def zero: Int = 0

    override def add(t: Int, other: Int): Int = t + other
  }

}

class GroupFor[M, T: Group](fn: T => M, coFn: M => T) extends MonoidFor[M, T](fn, coFn) with Group[M] {
  val groupT = implicitly[Group[T]]

  override def inverse(t: M) = fn(groupT.inverse(coFn(t)))
}

trait Functor[M[_]] {
  def fmap[A, B](m: M[A], fn: A => B): M[B]
}

object Functor {

  implicit object SeqAsFunctor extends Functor[Seq] {
    override def fmap[A, B](m: Seq[A], fn: (A) => B): Seq[B] = m.map(fn)
  }

}

object FunctionalLanguage {

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

}