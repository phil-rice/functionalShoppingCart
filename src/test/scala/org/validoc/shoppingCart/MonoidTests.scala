package org.validoc.shoppingCart

import scala.reflect.ClassTag

import Monoid._

abstract class AbstractMonoidTest[T: ClassTag : Monoid] extends ShoppingCartSpec {

  val monoid = implicitly[Monoid[T]]

  def name = implicitly[ClassTag[T]].runtimeClass.getSimpleName

  def zero: T

  def two: T

  def four: T

  def six: T

  behavior of name

  it should "implement zero" in {
    monoid.zero shouldBe zero
  }

  it should "implement simple addition" in {
    zero + two shouldBe two
    two + zero shouldBe two

    zero + four shouldBe four
    four + zero shouldBe four

    two + two shouldBe four
    two + four shouldBe six
  }

  it should "implement integer times" in {
    zero * 2 shouldBe zero
    two * 2 shouldBe four
    two * 3 shouldBe six
  }

  it should "implement subtract" in {
    two - zero shouldBe two
    four - two shouldBe two
    six - two shouldBe four
  }
}

class MoneyTest extends AbstractMonoidTest[Money] {
  override def zero: Money = Money(0)

  override def two: Money = Money(2)

  override def four: Money = Money(4)

  override def six: Money = Money(6)
}

class RewardTest extends AbstractMonoidTest[Reward] {
  override def zero: Reward = Reward(0)

  override def two: Reward = Reward(2)

  override def four: Reward = Reward(4)

  override def six: Reward = Reward(6)
}
