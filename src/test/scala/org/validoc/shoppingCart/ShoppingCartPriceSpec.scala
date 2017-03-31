package org.validoc.shoppingCart

import org.mockito.Mockito._
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

class ShoppingCartPriceSpec extends Spec with ShoppingCartFixture {

  behavior of "ShoppingCardDetails"

  def setup(fn: ShoppingCartResult => Unit) =
    setupDetails { details =>
      implicit offer =>
       fn(ShoppingCartPriceCalculator(details))
    }

  it should "keep the ids" in {
    setup { result =>
      result.ids shouldBe ids
    }
  }
  it should "calculate the originalPrice price as the sum of the sku items" in {
    setup { result =>
      result.originalPrice shouldBe appleSku.price + orangeSku.price
      result.originalPrice shouldBe Money(70)
    }
  }

  it should "have the discounts as those returned by the composite offer" in {
    setup { result =>
      result.discounts shouldBe foundDiscounts
    }
  }

  it should "calculate the savings as the sum of the discount prices" in {
    setup { result =>
      result.savings shouldBe discount("one").price + discount("two").price
      result.savings shouldBe Money(20)
    }
  }

  it should "calculate the price " in {
    setup { result =>
      result.price shouldBe Money(50)
    }
  }
}
