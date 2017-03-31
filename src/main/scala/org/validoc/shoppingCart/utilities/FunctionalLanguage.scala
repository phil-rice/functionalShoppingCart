package org.validoc.shoppingCart.utilities


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
