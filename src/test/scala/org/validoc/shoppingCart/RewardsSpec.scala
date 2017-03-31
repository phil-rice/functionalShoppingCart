package org.validoc.shoppingCart

import org.mockito.Mockito._

class BaseRewardCalculatorSpec extends Spec {
  behavior of "BaseRewardCalculator"

  it should "return the rewards as 1/100 of the money" in {
    BaseRewardCalculator(Money(0)) shouldBe Reward(0)
    BaseRewardCalculator(Money(99)) shouldBe Reward(0)
    BaseRewardCalculator(Money(100)) shouldBe Reward(1)
    BaseRewardCalculator(Money(900)) shouldBe Reward(9)
  }

}

class DoublePointsForSpec extends Spec with ShoppingCartFixture {

  behavior of "DoublePointsFor"

  it should "add up the price of the skus in the shopping basket that match the id and return the base reward calculator's result for that price" in {
    implicit val baseRewardCalculator = mock[BaseRewardCalculator]
    when(baseRewardCalculator.apply(Money(30))) thenReturn Reward(111)
    when(baseRewardCalculator.apply(Money(60))) thenReturn Reward(222)
    val pointsFor = new DoublePointsFor(appleSku)
    pointsFor(Seq(apple, orange)) shouldBe Reward(111)
    pointsFor(Seq(apple, apple, orange)) shouldBe Reward(222)
  }
}

class RewardCalculatorSpec extends Spec with ShoppingCartFixture {

  behavior of "RewardCalculator"

  def setup(fn: BaseRewardCalculator => BonusRewardCalculator => Unit): Unit = {
    val baseRewardCalculator = mock[BaseRewardCalculator]
    val bonusRewardCalculator = mock[BonusRewardCalculator]
    fn(baseRewardCalculator)(bonusRewardCalculator)
  }

//  it should "return the base reward calculator and any doubles" in {
//    setupDetails { cart =>
//      offer =>
//        setup { implicit base =>
//          implicit bonus =>
//            val result = ShoppingCartPriceCalculator(cart)
//            when(base.apply(result.price)) thenReturn Reward(111)
//            when(bonus.apply(result.items)) thenReturn Reward(222)
//            RewardCalculator(cart) shouldBe Reward(333)
//        }
//    }
//  }
}