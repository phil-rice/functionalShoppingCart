package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.Monoid
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

//I'm using implicits as a dependency injection mechanism.
// This is obviously a project specific choice
trait RewardCalculator {
  def apply(result: ShoppingCartResult)(implicit baseRewardCalculator: BaseRewardCalculator, bonusRewardCalculator: BonusRewardCalculator): Reward =
    baseRewardCalculator(result.price) + bonusRewardCalculator(result.ids)
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

class DoublePointsFor(sku: Sku)(implicit baseRewardCalculator: BaseRewardCalculator) extends BonusRewardCalculator {
  override def apply(ids: Seq[Id]): Reward = {
    val matchingIds = ids.filter(_ == sku.id)
    val matchingPrice = sku.price * matchingIds.size
    val bonusPoints = baseRewardCalculator(matchingPrice)
    bonusPoints
  }
}
