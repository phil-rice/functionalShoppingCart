package org.validoc.shoppingCart

//Both Money and Reward are naturally Monoids. By making this a type class
//I can control the usage of arithmetic on them, and make it really easy to change their representation
//In money's case that is very likely!
//A problem with typeclasses in Scala is that they are a pain to use, so the MonoidPimper is intended to help with that
trait Monoid[T] {
  def zero: T

  def add(t: T, other: T): T

  def subtract(t: T, other: T): T

  def timesBy(t: T, num: Int): T

}

object Monoid {

  implicit object MonoidForMoney extends Monoid[Money] {
    def zero = Money(0)

    def add(money: Money, other: Money) = Money(money.amount + other.amount)

    def timesBy(money: Money, num: Int): Money = Money(money.amount * num)

    override def subtract(t: Money, other: Money): Money = Money(t.amount - other.amount)
  }

  implicit object MonoidForReward extends Monoid[Reward] {
    override def zero: Reward = Reward(0)

    override def add(t: Reward, other: Reward): Reward = Reward(t.points + other.points)

    override def subtract(t: Reward, other: Reward): Reward = Reward(t.points - other.points)

    override def timesBy(t: Reward, num: Int): Reward = Reward(t.points * num)
  }

  implicit class MonoidPimper[T: Monoid](t: T) {
    val monoid = implicitly[Monoid[T]]

    def +(other: T) = monoid.add(t, other)

    def -(other: T) = monoid.subtract(t, other)

    def *(n: Int) = monoid.timesBy(t, n)
  }

  implicit class MonoidSeqPimper[T: Monoid](ts: Seq[T]) {
    val monoid = implicitly[Monoid[T]]

    import monoid._

    def addUp = ts.foldLeft(zero)(add)
  }

}

//A tiny type for money. Representing money is hard, so this encapsulates all uses of money
//These could be made AnyVals, but that makes the testing harder: Mockito doesn't understand AnyVals
case class Money(amount: Int)

case class Reward(points: Int)