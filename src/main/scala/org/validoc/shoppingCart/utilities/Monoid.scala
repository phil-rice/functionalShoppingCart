package org.validoc.shoppingCart.utilities

// Money, Rewards, Offers and ShoppingCarts are naturally Monoids. By making this a type class
//I can control the usage of arithmetic on them, and make it really easy to change their representation
//In money and offers case that is very likely!
trait Monoid[T] {
  def zero: T

  def add(t: T, other: T): T

  def timesBy(t: T, num: Int): T = List.fill(num)(t).foldLeft(zero)(add)
}

