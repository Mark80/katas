package gildrose

import cats.data.State

object UpdateAlgebra {

  def take(item: Item): State[Item, Item] =
    State.pure[Item, Item](item)

  def decreaseSellIn(item: Item): State[Item, Item] =
    State((item: Item) => {
      val itemN = item.copy(sellIn = item.sellIn - 1.days)
      (itemN, itemN)
    })

  def increaseQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val quality =
        if (item.quality < 50)
          item.quality + 1
        else
          item.quality

      val itemN = item.copy(quality = quality)
      (itemN, itemN)
    })

  def decreaseQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val quality =
        if (item.quality > 0)
          item.quality - 1
        else
          item.quality

      val itemN = item.copy(quality = quality)
      (itemN, itemN)
    })

  def setToZeroQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val itemN = item.copy(quality = 0)
      (itemN, itemN)
    })

  def updateQualityOfBackStagePass(item: Item): State[Item, Item] =
    for {
      item1 <- increaseQuality(item)
      item2 <- if (item1.sellIn < 11.days) increaseQuality(item1) else take(item1)
      item3 <- if (item2.sellIn < 6.days) increaseQuality(item2) else take(item2)
    } yield item3

  implicit class DayDsl(sellIn: Int) {
    def days: Int = sellIn
  }

}
