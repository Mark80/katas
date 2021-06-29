package ecommerce

object models {

  object customer {
    case class CustomerId(value : Int)

  }

  object payments {

    type CardNumber = Int
    type CheckNumber = String

    sealed trait CardType
    case object Visa extends CardType
    case object MasterVCard extends CardType

    case class CreditCardInfo(cardType: CardType, cardNumber: CardNumber)

    sealed trait PaymentMethod
    case object Cash extends PaymentMethod
    case class Check(checkNumber: CheckNumber, card: CreditCardInfo)
    case class Card(cardInfo: CreditCardInfo)

    type PaymentAmount = Double

    sealed trait Currency
    case object USD extends Currency
    case object EUR extends Currency

    case class Payment(paymentMethod: PaymentMethod, currency: Currency, paymentAmount: PaymentAmount)
  }

  object orders {

    case class OrderId(value : Int)

    case class CustomerInfo()
    case class ShippingAddress()
    case class AmountToBill()
    case class BillingAddress()
    case class BillingAmount()
    case class OrderLine()

    case class Order(
                      customerInfo : CustomerInfo,
                      shippingAddress : ShippingAddress,
                      billingAddress : BillingAddress,
                      orderLines : List[OrderLine],
                      amountToBill : BillingAmount
                    )

    case class UnvalidatedOrder()
    case class ValidatedOrder()
    case class PricedOrder()


   case class ValidationError(fieldName : String,
                             errroDescription : String)
    val validatedOrder : UnvalidatedOrder => Either[String, ValidatedOrder] = ???

    case class AcknowledgmentSent()
    case class OrderPlaced()
    case class BillableOrderPlaced()

    case class PlaceOrderEvents(
                               acknowledgmentSent: AcknowledgmentSent,
                               orderPlaced: OrderPlaced,
                               billableOrderPlaced: BillableOrderPlaced
                               )

    val placeOrder : UnvalidatedOrder => PlaceOrderEvents = ???

    sealed trait ProductCode
    case class WidgetCode(code : String)
    case class GizmoCode(code :String)

    sealed trait OrderQuantity
    case class UnitQuantity(q:Int)
    case class KilogramQuantity(q:Double)

    type EnvelopeContents = String

    sealed trait CategorizedMail
    case class QuoteForm() extends CategorizedMail
    case class OrderForm() extends CategorizedMail

    val categorizeInboundMail : EnvelopeContents => CategorizedMail = ???

    case class ProductCatalog()

    val calculatePrice : ProductCatalog => OrderForm  => PricedOrder = ???




  }
}
