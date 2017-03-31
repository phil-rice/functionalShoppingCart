package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.GroupFor

//A tiny type for money. Representing money is hard, so this encapsulates all uses of money
//These could be made AnyVals, but that makes the testing harder: Mockito doesn't understand AnyVals
case class Money(amount: Int)

object Money {

  implicit object GroupForMoney extends GroupFor[Money, Int](Money(_), _.amount)

}

case class Reward(points: Int)

object Reward {

  implicit object GroupForReward extends GroupFor[Reward, Int](Reward(_), _.points)

}