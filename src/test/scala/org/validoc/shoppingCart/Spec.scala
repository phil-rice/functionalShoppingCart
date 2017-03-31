package org.validoc.shoppingCart

import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

trait Spec extends FlatSpec with Matchers with MockitoSugar

trait ShoppingCartFixture extends MockitoSugar {

  val apple = Id("apple")
  val appleSku = Sku(apple, Description("Delicious Juicy Apples"), Money(30))

  val orange = Id("orange")
  val orangeSku = Sku(orange, Description("Over the top Orangy Goodness"), Money(40))

  implicit val skus = Map(apple -> appleSku, orange -> orangeSku)

  def discount(description: String) = Discount(Description(description), Money(10))


  val foundDiscounts = Seq(discount("one"), discount("two"))
  val ids = Seq(apple, orange)

  def setupDetails(fn: ShoppingCart => CompositeOffer => Unit) = {
    implicit val compositeOffer = mock[CompositeOffer]
    when(compositeOffer.apply(ids)) thenReturn foundDiscounts
    fn(ids)(compositeOffer)
  }
}
