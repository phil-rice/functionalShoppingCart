package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.FunctionalLanguage._
import org.validoc.shoppingCart.utilities.{Monoid, MonoidFor, Sequence}


//Like actual supermarkets, offers are 'a discount'
//So you get charged the full price and then there is a discount for the amount you save
trait Offer extends (Seq[Id] => Seq[Discount])

object Offer {
  def apply(offer: Offer*) = CompositeOffer(offer)
}


case class CompositeOffer(offers: Seq[Offer]) extends Offer {
  override def apply(ids: Seq[Id]): Seq[Discount] = offers.flatMap(_ apply ids)
}

object CompositeOffer {

  implicit object MonoidForCompositeOffer extends MonoidFor[CompositeOffer, Seq[Offer]](CompositeOffer(_), _.offers)

}

case class BuyNForY(sku: Sku, n: Int, price: Money) extends Offer {
  def matchingId(ids: Seq[Id]) = ids.filter(_ == sku.id)

  override def apply(ids: Seq[Id]): Seq[Discount] = {
    matchingId(ids).size / n match {
      case 0 => Seq()
      case timesOfferApplies =>
        val discountedPrice = price * timesOfferApplies
        val discount = sku.price * timesOfferApplies * n - discountedPrice
        Seq(Discount(Description(s"Bought ${timesOfferApplies * n} of ${sku.description.s} for $discountedPrice you have saved ${discount}"), discount))
    }
  }
}