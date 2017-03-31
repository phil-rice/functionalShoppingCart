package org.validoc.shoppingCart.utilities

trait Functor[M[_]] {
  def fmap[A, B](m: M[A], fn: A => B): M[B]
}


