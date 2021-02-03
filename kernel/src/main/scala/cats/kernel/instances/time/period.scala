package cats.kernel.instances.time

import cats.kernel.Show
import java.time.Period

trait period {
  implicit final val periodInstances =
    new Show[Period]{
      override def show(x: Period): String = x.toString
    }
}

object period extends period