import scala.util.Random

def infetta_al_50_percento = {
  Random.nextBoolean()
}


val risultati = (100 to 10000).map { numero_di_abitanti =>
    var infetti = 0.0
    (0 to numero_di_abitanti).foreach(_ =>

      if (infetta_al_50_percento)
        infetti += 1
    )
  (numero_di_abitanti, (infetti * 100)/numero_di_abitanti)
}

val più_infetti = risultati.maxBy{
  case (a: Int,b) => b
}

val meno_infetti = risultati.minBy{
  case (a: Int,b) => b
}