import cats.data.{StateT, NonEmptyList}
import cats.syntax.all._
import java.time.LocalTime

type ThrowableOr[A] = Either[Throwable, A]

object TableReservationSystem {

  final case class ReservationId(tableNumber: Int, hour: LocalTime)
  final case class Reservation(id: ReservationId, name: String)
  final case class Reservations(reservations: List[Reservation]) {
    def insert(reservation: Reservation): ThrowableOr[Reservations] =
      if (reservations.exists(r => r.id == reservation.id))
        Left(new TableAlreadyReservedException(reservation))
      else Right(Reservations(reservations :+ reservation))
  }

  final class TableAlreadyReservedException(
      reservation: Reservation
  ) extends RuntimeException(
        s"${reservation.name} cannot be added because table number ${reservation.id.tableNumber} is already reserved for the ${reservation.id.hour}"
      )

  val emptyReservationSystem: Reservations = Reservations(List.empty)

  def insertBooking(
      reservation: Reservation
  ): StateT[ThrowableOr, Reservations, Unit] =
    StateT.modifyF[ThrowableOr, Reservations](_.insert(reservation))

  def processBookings(
      bookings: NonEmptyList[Reservation]
  ): ThrowableOr[Reservations] =
    bookings
      .traverse_(insertBooking)
      .runS(emptyReservationSystem)
}