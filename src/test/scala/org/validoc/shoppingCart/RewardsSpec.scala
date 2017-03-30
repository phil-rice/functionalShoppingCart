package org.validoc.shoppingCart

import org.mockito.Mockito._

class BaseRewardCalculatorSpec extends ShoppingCartSpec {
  behavior of "BaseRewardCalculator"

  it should "return the rewards as 1/100 of the money" in {
    BaseRewardCalculator(Money(0)) shouldBe Reward(0)
    BaseRewardCalculator(Money(99)) shouldBe Reward(0)
    BaseRewardCalculator(Money(100)) shouldBe Reward(1)
    BaseRewardCalculator(Money(900)) shouldBe Reward(9)
  }

}

class DoublePointsForSpec extends ShoppingCartSpec with ShoppingCartFixture {

  behavior of "DoublePointsFor"

  it should "add up the price of the skus in the shopping basket that match the id and return the base reward calculator's result for that price" in {
    val baseRewardCalculator = mock[BaseRewardCalculator]
    when(baseRewardCalculator.apply(Money(30))) thenReturn Reward(111)
    when(baseRewardCalculator.apply(Money(60))) thenReturn Reward(222)
    val pointsFor = new DoublePointsFor(appleSku, baseRewardCalculator)
    pointsFor(Seq(apple, orange)) shouldBe Reward(111)
    pointsFor(Seq(apple, apple, orange)) shouldBe Reward(222)
  }
}

class RewardCalculatorSpec extends ShoppingCartSpec with ShoppingCartFixture {

  behavior of "RewardCalculator"

  def setup(fn: (BaseRewardCalculator, BonusRewardCalculator) => Unit): Unit = {
    val baseRewardCalculator = mock[BaseRewardCalculator]
    val bonusRewardCalculator = mock[BonusRewardCalculator]
    fn(baseRewardCalculator, bonusRewardCalculator)
  }

  it should "return the base reward calculator and any doubles" in {
    setupDetails { (details, _) =>
      setup { (base, bonus) =>
        when(base.apply(details.price)) thenReturn Reward(111)
        when(bonus.apply(details.ids)) thenReturn Reward(222)
        RewardCalculator(base, bonus)(details) shouldBe Reward(333)
      }
    }
  }
}