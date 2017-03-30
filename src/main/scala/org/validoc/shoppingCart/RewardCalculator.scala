package org.validoc.shoppingCart

import Monoid._

trait RewardCalculator {
  def apply(baseRewardCalculator: BaseRewardCalculator, bonusRewardCalculator: BonusRewardCalculator)(details: ShoppingCartDetails): Reward =
    baseRewardCalculator(details.price) + bonusRewardCalculator(details.ids)
}

object RewardCalculator extends RewardCalculator

trait BaseRewardCalculator extends (Money => Reward) {
  def apply(cost: Money): Reward = Reward(cost.amount / 100)
}

object BaseRewardCalculator extends BaseRewardCalculator

trait BonusRewardCalculator extends (Seq[Id] => Reward)

object NullBonusRewardCalculator extends BonusRewardCalculator {
  override def apply(id: Seq[Id]): Reward = implicitly[Monoid[Reward]].zero
}

class DoublePointsFor(sku: Sku, baseRewardCalculator: BaseRewardCalculator = BaseRewardCalculator) extends BonusRewardCalculator {
  override def apply(ids: Seq[Id]): Reward = {
    val matchingIds = ids.filter(_ == sku.id)
    val matchingPrice = sku.price * matchingIds.size
    val bonusPoints = baseRewardCalculator(matchingPrice)
    bonusPoints
  }
}