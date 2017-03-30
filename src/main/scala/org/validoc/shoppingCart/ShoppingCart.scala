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

//Again implicits as dependancy inject

object ShoppingCartDetails {
  def apply(ids: Seq[Id])(implicit skus: Map[Id, Sku], offers: CompositeOffer): ShoppingCartDetails = {
    val items = ids.map(skus)
    val discount = offers(ids)
    ShoppingCartDetails(items, discount)
  }

}

case class ShoppingCartDetails(items: Seq[Sku], discounts: Seq[Discount]) {
  val ids = items.map(_.id)
  val originalPrice = items.map(_.price).addUp
  val savings = discounts.map(_.price).addUp
  val price = originalPrice - savings
}
