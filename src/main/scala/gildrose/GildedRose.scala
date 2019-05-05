package gildrose

import cats.data.State

class GildedRose() {

  def updateQuality(items: Array[Item]): Array[Item] =
    items.map { item =>
      updateItem(item)
    }

  def updateItem(item: Item): Item =
    item.name match {

      case "Aged Brie" =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- increaseQuality(item1)
          item3 <- if (item2.sellIn < 0) increaseQuality(item2) else take(item2)
        } yield item3).result(item)

      case "Backstage passes to a TAFKAL80ETC concert" =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- if (item1.sellIn < 0) setToZeroQuality(item1) else updateQualityOfBackStagePass(item1)
        } yield item2).result(item)

      case genericItem if genericItem != "Sulfuras, Hand of Ragnaros" =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- decreaseQuality(item1)
          item3 <- if (item2.sellIn < 0) decreaseQuality(item2) else take(item2)
        } yield item3).result(item)

      case _ => item
    }

  private def take(item: Item): State[Item, Item] =
    State.pure[Item, Item](item)

  private def decreaseSellIn(item: Item): State[Item, Item] =
    State((item: Item) => {
      val itemN = item.copy(sellIn = item.sellIn - 1)
      (itemN, itemN)
    })

  private def increaseQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val quality =
        if (item.quality < 50)
          item.quality + 1
        else
          item.quality

      val itemN = item.copy(quality = quality)
      (itemN, itemN)
    })

  private def decreaseQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val quality =
        if (item.quality > 0)
          item.quality - 1
        else
          item.quality

      val itemN = item.copy(quality = quality)
      (itemN, itemN)
    })

  private def setToZeroQuality(item: Item): State[Item, Item] =
    State((item: Item) => {
      val itemN = item.copy(quality = 0)
      (itemN, itemN)
    })

  private def updateQualityOfBackStagePass(item: Item): State[Item, Item] =
    for {
      item1 <- increaseQuality(item)
      item2 <- if (item1.sellIn < 6) increaseQuality(item) else take(item1)
      item3 <- if (item2.sellIn < 11) increaseQuality(item2) else take(item2)
    } yield item3

  implicit class ItemDsl(state: State[Item, Item]) {

    def result(item: Item): Item = state.run(item).value._2

  }
}

case class Item(name: String, sellIn: Int, quality: Int)
