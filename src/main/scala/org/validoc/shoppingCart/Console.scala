package org.validoc.shoppingCart

import scala.annotation.tailrec
import org.validoc.shoppingCart.utilities.FunctionalLanguage._

trait MessageUser {
  def apply(s: Any): Unit
}

object MessageUserByPrintln extends MessageUser {
  override def apply(s: Any): Unit = println(s)
}

object Commands {
  implicit def charToId(char: Char) = Id(char.toString)

  //There is a tempation to do this using the command pattern and having one command per object. I think this is more readable at the moment, although
  //for production code I would split them out
  def command(command: Char, details: ShoppingCart)(implicit skus: Map[Id, Sku], compositeOffer: CompositeOffer, messageUser: MessageUser): ShoppingCart =
  (command, details) match {
    case ('0', _) => Seq()
    case ('q', details) => {
      messageUser("thank you for shopping")
      System.exit(0)
      details
    }
    case (command, details) if (skus.contains(command: Id)) => details + command
    case cmd => messageUser(s"Command ['$cmd'] not understood. Pressing q will exit"); details
  }

}

object ConsoleApp extends App {

  import Commands._

  implicit def tupleToSku(t: (Char, String, Int)) = Sku(t._1, Description(t._2), Money(t._3))

  private val apple: Sku = ('a', "Appealing Apples", 30)
  private val bacon: Sku = ('b', "Best Back Bacon", 40)
  private val carrots: Sku = ('c', "Crunchy Carrots", 50)
  private val donuts: Sku = ('d', "Delicious Donuts", 50)
  val skuList = List[Sku](apple, bacon, carrots, donuts)

  implicit val skus = skuList.foldLeft(Map[Id, Sku]())((acc, sku) => acc + (sku.id -> sku))
  implicit val offers = Offer(BuyNForY(apple, 2, Money(45)))
  implicit val baseRewardCalculator = BaseRewardCalculator
  implicit val bonusRewardCalculator = new DoublePointsFor(apple)
  implicit val messageUser = MessageUserByPrintln

  import sys.process._

  (Seq("sh", "-c", "stty -icanon min 1 < /dev/tty") !)
  (Seq("sh", "-c", "stty -echo < /dev/tty") !)

  messageUser("Usage: Press letters a,b,c,d to buy an apple, bacon, carrots or donuts. Press q to exit")

  @tailrec
  def processCommand(cart: ShoppingCart = Seq()): Unit = {
    val ch = Console.in.read.toChar
    val nextCart = command(ch, cart)
    val discounts = implicitly[CompositeOffer] apply nextCart
    val details = ShoppingCartResult(nextCart.fmap(implicitly[Map[Id, Sku]]), discounts)
    details.prettyPrint
    messageUser(RewardCalculator(details))
    messageUser("")
    processCommand(nextCart)
  }

  processCommand()

}
