package cats.kernel.instances.time

import cats.{Hash, Order, Show}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME

trait localdatetime {
  final def showLocalDateTime(formatter: DateTimeFormatter): Show[LocalDateTime] =
    Show[String].contramap(_.format(formatter))

  implicit final val localdatetimeInstances = 
    new Show[LocalDateTime] with Order[LocalDateTime] with Hash[LocalDateTime]{
      override def hash(x: LocalDateTime): Int = x.hashCode
      override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
      override def show(x: LocalDateTime): String = x.format(ISO_LOCAL_DATE_TIME)
    }
}

object localdatetime extends localdatetime