package cats.kernel.instances.time

import cats.{Hash, Order, Show}
import java.time.OffsetTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_OFFSET_TIME

trait offsettime {
  final def showOffsetTime(formatter: DateTimeFormatter): Show[OffsetTime] =
    Show[String].contramap(_.format(formatter))

  implicit final val offsettimeInstances = 
    new Show[OffsetTime] with Order[OffsetTime] with Hash[OffsetTime]{
      override def hash(x: OffsetTime): Int = x.hashCode
      override def compare(x: OffsetTime, y: OffsetTime): Int = x.compareTo(y)
      override def show(x: OffsetTime): String = x.format(ISO_OFFSET_TIME)
    }

}

object offsettime extends offsettime