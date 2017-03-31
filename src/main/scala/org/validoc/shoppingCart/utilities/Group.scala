package org.validoc.shoppingCart.utilities

trait Group[T] extends Monoid[T] {
  def inverse(t: T) = t

  def subtract(t: T, other: T): T = add(t, inverse(other))
}
