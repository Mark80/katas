package katadeliveryservice

import org.scalatest.{Matchers, WordSpec}

class DeliveryServiceSpec extends WordSpec with Matchers {

  "An User" should {

    "get all delivery  point" in {

      val deliveryService = new DeliveryService()

      val deliveryPoint = deliveryService.all

      deliveryPoint should contain theSameElementsAs List(DeliveryPoint("id1", "hub"), DeliveryPoint("id2", "locker"))

    }

  }

}
