//package booking
//
//import cats.Id
//import org.scalatest.{Matchers, WordSpec}
//
//class HotelServiceSpec extends WordSpec with Matchers {
//
//  val hotelService = new HotelService[Id]
//  implicit val repository: RepoTest = new RepoTest
//
//  "Hotel manager can add an hotel" should {
//
//    "find by id" in {
//      val hotelId = "id"
//      val hotelName = "name"
//      val newHotel = Hotel(hotelId, hotelName)
//
//      val result = for {
//        _ <- hotelService.addHotel(hotelId, hotelName)
//        hotel <- hotelService.findHotelBy(hotelId)
//      } yield hotel
//
//      result shouldBe newHotel
//    }
//  }
//
//}
//
//class RepoTest extends Repository[Id, Hotel] {
//  private var hotels = Map.empty[String, Hotel]
//  def add(hotel: Hotel) = hotels = hotels + (hotel.id -> hotel)
//  def find(id: String) = hotels.get(id)
//}
