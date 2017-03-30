package org.validoc.shoppingCart

import Monoid._


trait Offer extends (Seq[Id] => Seq[Discount])

object Offer {
  def apply(offer: Offer*) = CompositeOffer(offer)
}

trait SingleSkuOffer extends Offer {
  def allMatching(id: Id)(ids: Seq[Id]) = ids.filter(_ == id)
}

object NullOffer extends Offer {
  override def apply(v1: Seq[Id]): Seq[Discount] = Seq()
}
case class CompositeOffer(offers: Seq[Offer]) extends Offer {
  override def apply(ids: Seq[Id]): Seq[Discount] = offers.flatMap(_ apply ids)
}

case class BuyNForY(sku: Sku, n: Int, price: Money) extends SingleSkuOffer {
  val matchingId = allMatching(sku.id) _

  override def apply(ids: Seq[Id]): Seq[Discount] = {
    val timesOfferApplies = matchingId(ids).size / n
    if (timesOfferApplies == 0) Seq() else {
      val discountedPrice = price * timesOfferApplies
      val discount = sku.price * timesOfferApplies * n - discountedPrice
      Seq(Discount(Description(s"Bought ${timesOfferApplies * n} of ${sku.description.s} for $discountedPrice you have saved ${discount}"), discount))
    }
  }
}