package org.validoc.shoppingCart

import org.validoc.shoppingCart.utilities.{Group, Monoid}
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

import scala.reflect.ClassTag

abstract class AbstractMonoidTest[T: ClassTag : Monoid] extends Spec {

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

}

abstract class AbstractGroupTest[T: Group : ClassTag] extends AbstractMonoidTest[T] {

  it should "implement subtract" in {
    two - zero shouldBe two
    four - two shouldBe two
    six - two shouldBe four
  }
}

class MoneyTest extends AbstractGroupTest[Money] {
  override def zero: Money = Money(0)

  override def two: Money = Money(2)

  override def four: Money = Money(4)

  override def six: Money = Money(6)
}

class RewardTest extends AbstractGroupTest[Reward] {
  override def zero: Reward = Reward(0)

  override def two: Reward = Reward(2)

  override def four: Reward = Reward(4)

  override def six: Reward = Reward(6)
}

class CompositeOfferTest extends AbstractMonoidTest[CompositeOffer] {
  val o1 = mock[Offer]
  val o2 = mock[Offer]
  val o3 = mock[Offer]
  val o4 = mock[Offer]
  val o5 = mock[Offer]
  val o6 = mock[Offer]

  override def zero: CompositeOffer = CompositeOffer(Seq())

  override def two: CompositeOffer = CompositeOffer(Seq(o1, o2))

  override def four: CompositeOffer = CompositeOffer(Seq(o1, o2, o1, o2))

  override def six: CompositeOffer = CompositeOffer(Seq(o1, o2, o1, o2, o1, o2))
}

class SeqTest extends AbstractMonoidTest[Seq[String]] {
  override def zero: Seq[String] = Seq()

  override def two: Seq[String] = Seq("1", "2")

  override def four: Seq[String] = Seq("1", "2", "1", "2")

  override def six: Seq[String] = Seq("1", "2", "1", "2", "1", "2")
}