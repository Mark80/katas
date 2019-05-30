package httpsfours

import java.util.UUID
import scala.collection.mutable.ListBuffer
import cats.effect.IO

case class Hut(name: String)
case class HutWithId(id: String, name: String)

final class HutRepository(private val huts: ListBuffer[HutWithId]) {
  val makeId: IO[String] = IO { UUID.randomUUID().toString }

  def getHut(id: String): IO[Option[HutWithId]] =
    IO { huts.find(_.id == id) }

  def addHut(hut: Hut): IO[String] =
    for {
      uuid <- makeId
      _ <- IO { huts += hutWithId(hut, uuid) }
    } yield uuid

  def updateHut(hutWithId: HutWithId): IO[Unit] =
    for {
      _ <- IO { huts -= hutWithId }
      _ <- IO { huts += hutWithId }
    } yield ()

  def deleteHut(hutId: String): IO[Unit] =
    IO { huts.find(_.id == hutId).foreach(h => huts -= h) }

  def hutWithId(hut: Hut, id: String): HutWithId =
    HutWithId(id, hut.name)
}
object HutRepository {
  def empty: IO[HutRepository] = IO { new HutRepository(ListBuffer()) }
}
