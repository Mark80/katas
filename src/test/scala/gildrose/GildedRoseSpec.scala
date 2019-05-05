package gildrose

import org.scalatest.{Matchers, WordSpec}

class GildedRoseSpec extends WordSpec with Matchers {

  "Update an Items" should {

    "update items" in {
      val item1 = Item("standard1", 3, 5)
      val item2 = Item("standard2", 2, 8)

      val List(uItem1, uItem2) = updateQuality(Array(item1, item2))

      uItem1.sellIn shouldBe 2
      uItem1.quality shouldBe 4

      uItem2.sellIn shouldBe 1
      uItem2.quality shouldBe 7
    }

    "decrease SellIn and quality of a standard item" in {

      val item = Item("standard", 3, 5)

      updateItem(item) shouldBe Item("standard", 2, 4)

    }

    "quality will be never less than zero" in {

      val item = Item("standard", 1, 0)
      updateItem(item) shouldBe Item("standard", 0, 0)

    }

    "Aged Brie actually increases in Quality the older it gets" in {
      val item = Item("Aged Brie", 3, 5)

      updateItem(item) shouldBe Item("Aged Brie", 2, 6)

    }

    "Aged Brie Quality can't be > 50" in {
      val item = Item("Aged Brie", 3, 50)

      updateItem(item) shouldBe Item("Aged Brie", 2, 50)

    }

    "Sulfuras never has to be sold and never decreases in Quality" in {

      val item = Item("Sulfuras, Hand of Ragnaros", 3, 5)
      updateItem(item) shouldBe item

    }

    "Backstage passes increases in Quality as it's SellIn value approache" in {

      val item = Item("Backstage passes to a TAFKAL80ETC concert", 13, 5)
      updateItem(item) shouldBe Item("Backstage passes to a TAFKAL80ETC concert", 12, 6)

    }

    "Backstage passes increases by 2 when there are 10 days or less" in {

      val item = Item("Backstage passes to a TAFKAL80ETC concert", 10, 5)
      updateItem(item) shouldBe Item("Backstage passes to a TAFKAL80ETC concert", 9, 7)

    }

    "Backstage passes increases by 3 when there are 5 days or less" in {

      val item = Item("Backstage passes to a TAFKAL80ETC concert", 4, 6)
      updateItem(item) shouldBe Item("Backstage passes to a TAFKAL80ETC concert", 3, 9)

    }

    "Backstage passes drops to 0 after the concert" in {

      val item = Item("Backstage passes to a TAFKAL80ETC concert", 0, 6)
      updateItem(item) shouldBe Item("Backstage passes to a TAFKAL80ETC concert", -1, 0)

    }

  }

  val updateItem: Item => Item =
    (item: Item) => new GildedRose().updateItem(item)

  val updateQuality: Array[Item] => List[Item] =
    (items: Array[Item]) => new GildedRose().updateQuality(items).toList

}
