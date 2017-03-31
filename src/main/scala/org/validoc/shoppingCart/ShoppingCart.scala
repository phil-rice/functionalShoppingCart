package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.{Functor, Monoid, Sequence}
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

trait Pricable {
  def description: Description

  def price: Money
}


case class Id(id: String) extends AnyVal

case class Description(s: String) extends AnyVal

case class Sku(id: Id, description: Description, price: Money) extends Pricable

case class Discount(description: Description, price: Money) extends Pricable

//Again implicits as dependancy inject


trait ShoppingCartPriceCalculator {
  def apply(cart: ShoppingCart)(implicit skus: Map[Id, Sku], offer: CompositeOffer) =
    ShoppingCartResult(cart.fmap(skus), offer(cart))
}

object ShoppingCartPriceCalculator extends  ShoppingCartPriceCalculator

case class ShoppingCartResult(items: Seq[Sku], discounts: Seq[Discount]) {
  val ids = items.map(_.id)
  val originalPrice = items.map(_.price).addUp
  val savings = discounts.map(_.price).addUp
  val price = originalPrice - savings

  def prettyPrint = {
    items.foreach { i => println(s"${i.description} for ${i.price}") }
    discounts.foreach { i => println(i.description) }
    println(s"Total: ${price} Savings ${savings}")
  }
}
