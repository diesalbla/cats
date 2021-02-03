package cats.kernel.instances.time

import cats.{Hash, Order, Show}
import java.time.Month
import java.time.format.TextStyle
import java.util.Locale

trait month {
  implicit final val monthInstances = new Show[Month] with Order[Month] with Hash[Month] {
    def compare(x: Month, y: Month): Int = x.compareTo(y)

    def hash(x: Month): Int = x.hashCode

    def show(t: Month): String = t.getDisplayName(TextStyle.SHORT, Locale.ENGLISH)
  }
}

object month extends month
