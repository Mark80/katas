package gildrose

import cats.data.State

case class Item(name: String, sellIn: Int, quality: Int)

object Item {

  implicit class ItemDsl(state: State[Item, Item]) {
    def result(item: Item): Item = state.run(item).value._2
  }

}
