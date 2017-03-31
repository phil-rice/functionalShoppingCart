package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.{Functor, Monoid}
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

object ShoppingCart {
  def apply(ids: Seq[Id])(implicit skus: Map[Id, Sku], offers: CompositeOffer): ShoppingCart = {
    val items = ids.map(skus)
    val discount = offers(ids)
    ShoppingCart(items, discount)
  }

  implicit object MonoidForShoppingCartDetails extends Monoid[ShoppingCart] {
    override def zero: ShoppingCart = ShoppingCart(Seq(), Seq())

    override def add(t: ShoppingCart, other: ShoppingCart): ShoppingCart = ShoppingCart(t.items ++ other.items, t.discounts ++ other.discounts)
  }


}

case class ShoppingCart(items: Seq[Sku], discounts: Seq[Discount]) {
  val ids = items.map(_.id)
  val originalPrice = items.map(_.price).addUp
  val savings = discounts.map(_.price).addUp
  val price = originalPrice - savings

  def +(id: Id)(implicit skus: Map[Id, Sku], offer: CompositeOffer) = ShoppingCart(ids ++ Seq(id))

  def prettyPrint = {
    items.foreach { i => println(s"${i.description} for ${i.price}") }
    discounts.foreach { i => println(i.description) }
    println(s"Total: ${price} Savings ${savings}")
  }
}
