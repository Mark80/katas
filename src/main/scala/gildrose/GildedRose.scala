package gildrose

import UpdateAlgebra._

class GildedRose() {

  val AgedBrie = "Aged Brie"
  val Backstage = "Backstage passes to a TAFKAL80ETC concert"
  val Sulfuras = "Sulfuras, Hand of Ragnaros"

  def updateQuality(items: Array[Item]): Array[Item] =
    items.map { item =>
      updateItem(item)
    }

  def updateItem(item: Item): Item =
    item.name match {

      case AgedBrie =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- increaseQuality(item1)
          item3 <- if (item2.sellIn < 0.days) increaseQuality(item2) else take(item2)
        } yield item3).result(item)

      case Backstage =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- if (item1.sellIn < 0.days) setToZeroQuality(item1) else updateQualityOfBackStagePass(item1)
        } yield item2).result(item)

      case genericItem if genericItem != Sulfuras =>
        (for {
          item1 <- decreaseSellIn(item)
          item2 <- decreaseQuality(item1)
          item3 <- if (item2.sellIn < 0.days) decreaseQuality(item2) else take(item2)
        } yield item3).result(item)

      case _ => item
    }

}
