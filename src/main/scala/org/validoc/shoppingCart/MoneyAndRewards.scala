package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.{Group, Monoid}

//A tiny type for money. Representing money is hard, so this encapsulates all uses of money
//These could be made AnyVals, but that makes the testing harder: Mockito doesn't understand AnyVals
case class Money(amount: Int)

object Money {

  implicit object GroupForMoney extends Group[Money] {
    def zero = Money(0)

    def add(money: Money, other: Money) = Money(money.amount + other.amount)

    override def timesBy(money: Money, num: Int): Money = Money(money.amount * num)

    override def subtract(t: Money, other: Money): Money = Money(t.amount - other.amount)
  }

}

case class Reward(points: Int)

object Reward {

  implicit object GroupForReward extends Group[Reward] {
    override def zero: Reward = Reward(0)

    override def add(t: Reward, other: Reward): Reward = Reward(t.points + other.points)

    override def subtract(t: Reward, other: Reward): Reward = Reward(t.points - other.points)

    override def timesBy(t: Reward, num: Int): Reward = Reward(t.points * num)
  }

}