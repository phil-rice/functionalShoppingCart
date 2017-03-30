package org.validoc.shoppingCart

import Monoid._

trait Pricable {
  def description: Description

  def price: Money
}

case class Id(id: String) extends AnyVal

case class Description(s: String) extends AnyVal

case class Sku(id: Id, description: Description, price: Money) extends Pricable

case class Discount(description: Description, price: Money) extends Pricable

case class ShoppingCartDetails(ids: Seq[Id])(implicit skus: Map[Id, Sku], offers: CompositeOffer) {
  val fullItems = ids.map(skus)
  val discounts = offers(ids)
  val originalPrice = fullItems.map(_.price).addUp
  val savings = discounts.map(_.price).addUp
  val price = originalPrice - savings
}
