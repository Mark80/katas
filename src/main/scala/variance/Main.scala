//package variance
//
//object Main extends App {
//
//  class Vehicle
//  class Car extends Vehicle
//  class Bus extends Vehicle
//
//  class IParking[T](things: List[T]) {
//    def park(vehicle: T): Unit                   = print("park")
//    def impound(vehicles: List[Vehicle]): Unit   = print("impound")
//    def checkVehicle(condition: String): List[T] = List.empty[T]
//  }
//
//  class CParking[+T](things: List[T]) {
//    def park[B >: T](vehicle: B): Unit           = print("park")
//    def impound[B >: T](vehicles: List[B]): Unit = print("impound")
//    def checkVehicle(condition: String): List[T] = List.empty[T]
//  }
//
//  class CVParking[-T](things: List[T]) {
//    def park(vehicle: T): Unit                           = print("park")
//    def impound(vehicles: List[T]): Unit                 = print("impound")
//    def checkVehicle[B <: T](condition: String): List[B] = List.empty[B]
//  }
//
////  class XCage[-T] {
////    val t: T = ???
////  }
//
//  val cageCar: XCage[Car] = new XCage[Vehicle] {
//    override val t: Vehicle = new Vehicle
//  }
//  val v: Car = cageCar.t
//
//}
