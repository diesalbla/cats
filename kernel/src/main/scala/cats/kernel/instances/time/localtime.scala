package cats.kernel.instances.time

import cats.kernel.{Hash, Order, Show}
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_LOCAL_TIME

trait localtime {
  final def showLocalTime(formatter: DateTimeFormatter): Show[LocalTime] =
    Show[String].contramap(_.format(formatter))

  implicit final val localtimeInstances = 
    new Show[LocalTime] with Order[LocalTime] with Hash[LocalTime]{
      override def hash(x: LocalTime): Int = x.hashCode
      override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
      override def show(x: LocalTime): String = x.format(ISO_LOCAL_TIME)
    }
}

object localtime extends localtime