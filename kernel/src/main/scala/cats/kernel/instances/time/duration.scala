package cats.kernel.instances.time

import cats.kernel.{Hash, Order, Show, CommutativeMonoid}
import java.time.Duration

trait duration {
  implicit final val durationInstances: Show[Duration]
    with Hash[Duration]
    with Order[Duration]
    with CommutativeMonoid[Duration] =
    new Hash[Duration] with Order[Duration] with Show[Duration] with CommutativeMonoid[Duration] {
      override def hash(x: Duration): Int = x.hashCode
      override def compare(x: Duration, y: Duration): Int = x.compareTo(y)
      override def show(x: Duration): String = x.toString
      override def empty: Duration = Duration.ZERO
      override def combine(x: Duration, y: Duration): Duration = x.plus(y)
    }
}

object duration extends duration
