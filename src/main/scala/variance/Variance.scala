package variance

object Variance extends App {

  class InvParking[T](things: List[T]) {
    def park(vehicle: T): InvParking[T] = ???
    def impound(vehicles: List[T]): InvParking[T] = ???
    def checkVehicles(condition: String): List[T] = ???
    def flatMap[S](f: T => InvParking[S]): InvParking[S] = ???
  }

  class CovParking[+T](things: List[T]) {
    def park[S >: T](vehicle: S): CovParking[S] = ???
    def impound[S >: T](vehicles: List[S]): CovParking[S] = ???
    def checkVehicles(condition: String): List[T] = ???
    def flatMap[S](f: T => CovParking[S]): CovParking[S] = ???
  }

  class ContraParking[-T](things: List[T]) {
    def park(vehicle: T): ContraParking[T] = ???
    def impound(vehicles: List[T]): ContraParking[T] = ???
    def checkVehicles[S <: T](condition: String): List[S] = ???
    def flatMap[S, U <: T](f: U => ContraParking[S]): ContraParking[S] = ???
  }

  class IList[T]

  class InvParkingI[T](things: IList[T]) {
    def park(vehicle: T): InvParkingI[T] = ???
    def impound(vehicles: IList[T]): InvParkingI[T] = ???
    def checkVehicles(condition: String): IList[T] = ???
    def flatMap[S](f: T => InvParkingI[S]): InvParkingI[S] = ???
  }

  class CovParkingI [+T](things: IList[T]) {
    def park[S >: T](vehicle: S): CovParkingI[S] = ???
    def impound[S >: T](vehicles: IList[S]): CovParkingI[S] = ???
    def checkVehicles[S >: T](condition: String): IList[S] = ???
    def flatMap[S](f: T => CovParkingI[S]): CovParkingI[S] = ???
  }

  class ContraParkingI[-T](things: IList[T]) {
    def park[S <: T](vehicle: S): ContraParkingI[S] = ???
    def impound[S <: T](vehicles: IList[S]): ContraParkingI[S] = ???
    def checkVehicles[S <: T](condition: String): IList[S] = ???
    def flatMap[S, U <: T](f: U => ContraParkingI[S]): ContraParkingI[S] = ???
  }

  println("compiled")
}
