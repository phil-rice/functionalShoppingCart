package org.validoc.shoppingCart

import org.mockito.Mockito._
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

class ShoppingCartSpec extends Spec with ShoppingCartFixture {

  behavior of "ShoppingCardDetails"

  it should "turn have ids " in {
    setupDetails { (details, compositeOffer) =>
      details.ids shouldBe ids
    }
  }

  it should "calculate the originalPrice price as the sum of the sku items" in {
    setupDetails { (details, compositeOffer) =>
      details.originalPrice shouldBe appleSku.price + orangeSku.price
      details.originalPrice shouldBe Money(70)
    }
  }

  it should "have the discounts as those returned by the composite offer" in {
    setupDetails { (details, compositeOffer) =>
      details.discounts shouldBe foundDiscounts
    }
  }

  it should "calculate the savings as the sum of the discount prices" in {
    setupDetails { (details, compositeOffer) =>
      details.savings shouldBe discount("one").price + discount("two").price
      details.savings shouldBe Money(20)
    }
  }

  it should "calculate the price " in {
    setupDetails { (details, compositeOffer) =>
      details.price shouldBe Money(50)
    }
  }
}
