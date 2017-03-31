package org.validoc.shoppingCart

import org.scalatest.mockito.MockitoSugar


class BuyNForYTests extends Spec with ShoppingCartFixture {

  behavior of "Buy 2 apples for 50"

  val offer = new BuyNForY(appleSku, 2, Money(50))

  it should "have a matching function that filter for the item" in {
    offer.matchingId(Seq(orange, apple, apple, orange, orange)) shouldBe Seq(apple, apple)
  }

  it should "produce no discount if there are zero apples " in {
    offer(Seq(orange, orange, orange)) shouldBe Seq()
  }

  it should "produce no discount if there is one apple " in {
    offer(Seq(orange, apple, orange, orange)) shouldBe Seq()
    offer(Seq(orange, orange, apple, orange)) shouldBe Seq()
  }

  it should "produce a single  discount if the offer applies once" in {
    val discount = Discount(Description("Bought 2 of Delicious Juicy Apples for Money(50) you have saved Money(10)"), Money(10))
    offer(Seq(orange, apple, apple, orange, orange)) shouldBe Seq(discount)
    offer(Seq(orange, orange, apple, apple, orange)) shouldBe Seq(discount)

  }

  it should "produce a single  discount if the offer applies multiple times" in {
    val discount = Discount(Description("Bought 4 of Delicious Juicy Apples for Money(100) you have saved Money(20)"), Money(20))
    offer(Seq(apple, apple, apple, apple, orange)) shouldBe Seq(discount)
  }
}


class CompositeOfferTests extends Spec with ShoppingCartFixture {

  import org.mockito.Mockito._

  val seq = Seq(apple, orange)

  def setup(fn: (Offer, Offer, Offer) => Unit) = {
    val offer1 = mock[Offer]
    val offer2 = mock[Offer]
    val offer3 = mock[Offer]
    makeMockReturn(offer1, "one")
    makeMockReturn(offer2, "two")
    makeMockReturn(offer3, "three")
    fn(offer1, offer2, offer3)
  }


  def makeMockReturn(offer: Offer, description: String) = when(offer.apply(seq)) thenReturn Seq(discount(description))

  behavior of "Composite offer"

  it should "present the ids to each offer and aggregate the results" in {
    setup { (offer1, offer2, offer3) =>
      Offer()(seq) shouldBe Seq()
      Offer(offer1)(seq) shouldBe Seq(discount("one"))
      Offer(offer1, offer2, offer3)(seq) shouldBe Seq(discount("one"), discount("two"), discount("three"))
    }
  }
}