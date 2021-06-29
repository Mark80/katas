package reactor

import reactor.core.publisher.Flux

import java.util

object Main extends App {

  val flux = Flux.fromArray(Array[String]("A", "B", "C"))

  flux
    .map(s => s.toLowerCase)
    .flatMap((s: String) => Flux.fromIterable(util.Arrays.asList(s.toCharArray))
    .subscribe(t => println(t))

}
