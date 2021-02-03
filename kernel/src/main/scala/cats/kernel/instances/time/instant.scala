package cats.kernel.instances.time

import cats.{Hash, Order, Show}
import java.time.Instant

trait instant {
  implicit final val instantInstances =
    new Hash[Instant] with Order[Instant] with Show[Instant] {
      override def hash(x: Instant): Int = x.hashCode
      override def compare(x: Instant, y: Instant): Int = x.compareTo(y)
      override def show(x: Instant): String = x.toString
    }

}

object instant extends instant