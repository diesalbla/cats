package cats.instances.nested

import cats.{Apply, Applicative, Traverse, Bitraverse, Alternative, Functor, Invariant, Monad, FlatMap, Contravariant, ~>, Show}
import cats.kernel.{Eq, Order, PartialOrder}
import cats.instances.nested.option.OptionT

object either {

  type EitherT[F[_], L, R] = F[Either[L, R]]

  implicit final class NestedEitherOps[F[_], A, B](val value: F[Either[A, B]]) extends AnyVal {

    def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

    def foldF[C](fa: A => F[C], fb: B => F[C])(implicit F: FlatMap[F]): F[C] = F.flatMap(value)(_.fold(fa, fb))

    def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

    def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

    def swap(implicit F: Functor[F]): EitherT[F, B, A] = F.map(value)(_.swap)

    def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

    def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
      F.flatMap(value) {
        case Left(_)  => default
        case Right(b) => F.pure(b)
      }

    def orElse[AA, BB >: B](default: => EitherT[F, AA, BB])(implicit F: Monad[F]): EitherT[F, AA, BB] =
      F.flatMap(value) {
        case Left(_)      => default
        case r @ Right(_) => F.pure(r.leftCast)
      }

    def recover(pf: PartialFunction[A, B])(implicit F: Functor[F]): EitherT[F, A, B] = F.map(value)(_.recover(pf))

    def recoverWith(pf: PartialFunction[A, EitherT[F, A, B]])(implicit F: Monad[F]): EitherT[F, A, B] =
      F.flatMap(value) {
        case Left(a) if pf.isDefinedAt(a) => pf(a)
        case other                        => F.pure(other)
      }

    def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F]): F[BB] = fold(f, identity)

    def valueOrF[BB >: B](f: A => F[BB])(implicit F: Monad[F]): F[BB] =
      F.flatMap(value) {
        case Left(a)  => f(a)
        case Right(b) => F.pure(b)
      }

    def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

    def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

    def ensure[AA >: A](onFailure: => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
      F.map(value)(_.ensure(onFailure)(f))

    def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
      F.map(value)(_.ensureOr(onFailure)(f))

    def toOption(implicit F: Functor[F]): OptionT[F, B] =
      F.map(value)(_.toOption)

    def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] =
      F.map(value)(_.to[G])

    def collectRight(implicit FA: Alternative[F], FM: Monad[F]): F[B] =
      FM.flatMap(value)(_.to[F])

    def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
      F.map(value)(_.bimap(fa, fb))

    def bitraverse[G[_], C, D](
      f: A => G[C],
      g: B => G[D])
      (implicit traverseF: Traverse[F],
        applicativeG: Applicative[G])
        : G[EitherT[F, C, D]] =
      traverseF.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g))

    def biflatMap[AA >: A, BB >: B](
      fa: A => EitherT[F, AA, BB],
      fb: B => EitherT[F, AA, BB])(implicit F: FlatMap[F]
    ): EitherT[F, AA, BB] =
      F.flatMap(value) {
        case Left(a)  => fa(a)
        case Right(b) => fb(a)
      }

    def applyAlt[D](ff: EitherT[F, A, B => D])(implicit F: Apply[F]): EitherT[F, A, D] =
      F.map2(value, ff)((xb, xbd) => Apply[Either[A, ?]].ap(xbd)(xb))

    def flatMap[AA >: A, D](f: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
      F.flatMap(value) {
        case l @ Left(_) => F.pure(l.rightCast)
        case Right(b)    => f(b).value
      }

    def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
      F.map(value)(f)

    def subflatMap[AA >: A, D](f: B => Either[AA, D])(implicit F: Functor[F]): EitherT[F, AA, D] =
      transform(_.flatMap(f))

    def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] = bimap(identity, f)

    /**
      * Modify the context `F` using transformation `f`.
      */
    def mapK[G[_]](f: F ~> G): EitherT[G, A, B] = EitherT[G, A, B](f(value))

    def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): EitherT[F, A, D] =
      F.map(flatMap(f))(Right(_))

    def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] = bimap(f, identity)

    def leftFlatMap[BB >: B, D](f: A => EitherT[F, D, BB])(implicit F: Monad[F]): EitherT[F, D, BB] =
      F.flatMap(value) {
        case Left(a)      => f(a).value
        case r @ Right(_) => F.pure(r.leftCast)
      }

    def leftSemiflatMap[D](f: A => F[D])(implicit F: Monad[F]): EitherT[F, D, B] =
      F.flatMap(value) {
        case Left(a)      => F.map(f(a))(d => Left(d) )
        case r @ Right(_) => F.pure(r.leftCast)
      }

    /** Combine `leftSemiflatMap` and `semiflatMap` together.
      *
      * Example:
      * {{{

      * scala> import cats.data.EitherT
      *
      * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Left("abc"), Right(123)))
      * scala> eitherT.biSemiflatMap(string => List(string.length), int => List(int.toFloat))
      * res0: cats.data.EitherT[List,Int,Float] = List(Left(3), Right(123.0))
      * }}}
      */
    def biSemiflatMap[C, D](fa: A => F[C], fb: B => F[D])(implicit F: Monad[F]): EitherT[F, C, D] =
      F.flatMap(value) {
        case Left(a)  => F.map(fa(a))(c => Left(c)  )
        case Right(b) => F.map(fb(b))(d => Right(d) )
      }

    def compare(that: EitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
      o.compare(value, that)

    def partialCompare(that: EitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
      p.partialCompare(value, that)

    def ===(that: EitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
      eq.eqv(value, that.value)

    def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F],
      applicativeG: Applicative[G]): G[EitherT[F, A, D]] =
      traverseF.traverse(value)(axb => Traverse[Either[A, ?]].traverse(axb)(f))

    def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
      F.foldLeft(value, c)((c, axb) => axb.foldLeft(c)(f))

    def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
      F.foldRight(value, lc)((axb, lc) => axb.foldRight(lc)(f))

    def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

    /**
      * Similar to `Either#combine` but mapped over an `F` context.
      *
      * Examples:
      * {{{
      * scala> import cats.data.EitherT

      * scala> val l1: EitherT[Option, String, Int] = EitherT.left(Some("error 1"))
      * scala> val l2: EitherT[Option, String, Int] = EitherT.left(Some("error 2"))
      * scala> val r3: EitherT[Option, String, Int] = EitherT.right(Some(3))
      * scala> val r4: EitherT[Option, String, Int] = EitherT.right(Some(4))
      * scala> val noneEitherT: EitherT[Option, String, Int] = EitherT.left(None)
      *
      * scala> l1 combine l2
      * res0: EitherT[Option, String, Int] = Some(Left(error 1))
      *
      * scala> l1 combine r3
      * res1: EitherT[Option, String, Int] = Some(Left(error 1))
      *
      * scala> r3 combine l1
      * res2: EitherT[Option, String, Int] = Some(Left(error 1))
      *
      * scala> r3 combine r4
      * res3: EitherT[Option, String, Int] = Some(Right(7))
      *
      * scala> l1 combine noneEitherT
      * res4: EitherT[Option, String, Int] = None
      *
      * scala> noneEitherT combine l1
      * res5: EitherT[Option, String, Int] = None
      *
      * scala> r3 combine noneEitherT
      * res6: EitherT[Option, String, Int] = None
      *
      * scala> noneEitherT combine r4
      * res7: EitherT[Option, String, Int] = None
      * }}}
      */
    def combine(that: EitherT[F, A, B])(implicit F: Apply[F], B: Semigroup[B]): EitherT[F, A, B] =
      F.map2(value, that)(_.combine(_))

    def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
      F.map(value)(_.toValidated)

    def toValidatedNel(implicit F: Functor[F]): F[ValidatedNel[A, B]] =
      F.map(value)(_.toValidatedNel)

    def toValidatedNec(implicit F: Functor[F]): F[ValidatedNec[A, B]] =
      F.map(value)(_.toValidatedNec)

    /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[EitherT]]`.
      *
      * The [[Applicative]] instance for `EitherT` "fails fast" - it is often useful to "momentarily" have
      * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
      *
      * Example:
      * {{{

      * scala> type Error = String
      * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 1")
      * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 2")
      * scala> val eithert: EitherT[Option, Error, Int] = EitherT.leftT[Option, Int]("error 3")
      * scala> eithert.withValidated { v3 => (v1, v2, v3.toValidatedNel).mapN { case (i, j, k) => i + j + k } }
      * res0: EitherT[Option, NonEmptyList[Error], Int] = Some(Left(NonEmptyList(error 1, error 2, error 3)))
      * }}}
      */
    def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB])(implicit F: Functor[F]): EitherT[F, AA, BB] =
      F.map(value)(either => f(either.toValidated).toEither)

    def show(implicit show: Show[F[Either[A, B]]]): String = show.show(value)

    /**
      * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Either[A, ?], B]`.
      *
      * An example where `toNested` can be used, is to get the `Apply.ap` function with the
      * behavior from the composed `Apply` instances from `F` and `Either[A, ?]`, which is
      * inconsistent with the behavior of the `ap` from `Monad` of `EitherT`.
      *
      * {{{
      * scala> import cats.data.EitherT

      * scala> val ff: EitherT[List, String, Int => String] =
      *      |   List(Either.right(_.toString), Either.left("error"))
      * scala> val fa: EitherT[List, String, Int] =
      *      |   List(Either.right(1), Either.right(2))
      * scala> ff.ap(fa)
      * res0: EitherT[List,String,String] = List(Right(1), Right(2), Left(error))
      * scala> (ff.toNested).ap(fa.toNested).value
      * res1: EitherT[List,String,String] = List(Right(1), Right(2), Left(error), Left(error))
      * }}}
      *
      */
    def toNested: Nested[F, Either[A, ?], B] = Nested[F, Either[A, ?], B](value)

    /**
      * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Validated[A, ?], B]`.
      *
      * Example:
      * {{{
      * scala> import cats.data.{EitherT, Validated}

      * scala> val f: Int => String = i => (i*2).toString
      * scala> val r1: EitherT[Option, String, Int => String] = EitherT.right(Some(f))
      * r1: cats.data.EitherT[Option,String,Int => String] = Some(Right(<function1>))
      * scala> val r2: EitherT[Option, String, Int] = EitherT.right(Some(10))
      * r2: cats.data.EitherT[Option,String,Int] = Some(Right(10))
      * scala> type ErrorOr[A] = Validated[String, A]
      * scala> (r1.toNestedValidated).ap(r2.toNestedValidated)
      * res0: cats.data.Nested[Option,ErrorOr,String] = Nested(Some(Valid(20)))
      * }}}
      */
    def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, ?], B] =
      Nested[F, Validated[A, ?], B](F.map(value)(_.toValidated))

    /**
      * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNel[A, ?], B]`.
      */
    def toNestedValidatedNel(implicit F: Functor[F]): Nested[F, ValidatedNel[A, ?], B] =
      Nested[F, ValidatedNel[A, ?], B](F.map(value)(_.toValidatedNel))

    /**
      * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNec[A, ?], B]`.
      */
    def toNestedValidatedNec(implicit F: Functor[F]): Nested[F, ValidatedNec[A, ?], B] =
      Nested[F, ValidatedNec[A, ?], B](F.map(value)(_.toValidatedNec))

  }

  object EitherT extends EitherTInstances {

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class LeftPartiallyApplied[B](private val dummy: Boolean = true) extends AnyVal {
      def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): EitherT[F, A, B] = F.map(fa)(Either.left)
    }

    /**
      * Creates a left version of `EitherT[F, A, B]` from a `F[A]`
      * {{{
      * scala> import cats.data.EitherT

      * scala> EitherT.left[Int](Option("err"))
      * res0: cats.data.EitherT[Option,String,Int] = Some(Left(err))
      * }}}
      */
    final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class LeftTPartiallyApplied[F[_], B](private val dummy: Boolean = true) extends AnyVal {
      def apply[A](a: A)(implicit F: Applicative[F]): EitherT[F, A, B] = F.pure(Either.left(a))
    }

    /**
      * Creates a left version of `EitherT[F, A, B]` from a `A`
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> EitherT.leftT[Option, Int]("err")
      * res0: EitherT[Option,String,Int] = Some(Left(err))
      * }}}
      */
    final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class RightPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
      def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = F.map(fb)(Either.right)
    }

    /**
      * Creates a right version of `EitherT[F, A, B]` from a `F[B]`
      * {{{
      * scala> import cats.data.EitherT

      * scala> EitherT.right[String](Option(3))
      * res0: cats.data.EitherT[Option,String,Int] = Some(Right(3))
      * }}}
      */
    final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class PurePartiallyApplied[F[_], A](private val dummy: Boolean = true) extends AnyVal {
      def apply[B](b: B)(implicit F: Applicative[F]): EitherT[F, A, B] = right(F.pure(b))
    }

    /**
      * Creates a new `EitherT[F, A, B]` from a `B`
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> EitherT.pure[Option, String](3)
      * res0: Option[Either[String, Int]] = Some(Right(3))
      * }}}
      */
    final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

    /**
      * Alias for [[pure]]
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> EitherT.rightT[Option, String](3)
      * res0: EitherT[Option,String,Int] = Some(Right(3))
      * }}}
      */
    final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure

    /**
      * Alias for [[right]]
      * {{{
      * scala> import cats.data.EitherT

      * scala> val o: Option[Int] = Some(3)
      * scala> val n: Option[Int] = None
      * scala> EitherT.liftF(o)
      * res0: cats.data.EitherT[Option,Nothing,Int] = Some(Right(3))
      * scala> EitherT.liftF(n)
      * res1: cats.data.EitherT[Option,Nothing,Int] = None
      * }}}
      */
    final def liftF[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

    /**
      * Same as [[liftF]], but expressed as a FunctionK for use with mapK
      * {{{
      * scala> import cats._, data._, implicits._
      * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
      * scala> val b: OptionT[EitherT[Eval, String, ?], Int] = a.mapK(EitherT.liftK)
      * scala> b.value.value.value
      * res0: Either[String,Option[Int]] = Right(Some(1))
      * }}}
      */
    final def liftK[F[_], A](implicit F: Functor[F]): F ~> EitherT[F, A, ?] =
      λ[F ~> EitherT[F, A, ?]](right(_))

    /** Transforms an `Either` into an `EitherT`, lifted into the specified `Applicative`.
      *
      * Note: The return type is a FromEitherPartiallyApplied[F], which has an apply method
      * on it, allowing you to call fromEither like this:
      * {{{

      * scala> val t: Either[String, Int] = Either.right(3)
      * scala> EitherT.fromEither[Option](t)
      * res0: EitherT[Option, String, Int] = Some(Right(3))
      * }}}
      *
      * The reason for the indirection is to emulate currying type parameters.
      */
    final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class FromEitherPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
      def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): EitherT[F, E, A] =
        F.pure(either)
    }

    /** Transforms an `Option` into an `EitherT`, lifted into the specified `Applicative` and using
      *  the second argument if the `Option` is a `None`.
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> val o: Option[Int] = None
      * scala> EitherT.fromOption[List](o, "Answer not known.")
      * res0: EitherT[List, String, Int] = List(Left(Answer not known.))
      * scala> EitherT.fromOption[List](Some(42), "Answer not known.")
      * res1: EitherT[List, String, Int] = List(Right(42))
      * }}}
      */
    final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
      def apply[E, A](opt: Option[A], ifNone: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
        F.pure(Either.fromOption(opt, ifNone))
    }

    /** Transforms an `F[Option]` into an `EitherT`, using the second argument if the `Option` is a `None`.
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> val o: Option[Int] = None
      * scala> EitherT.fromOptionF(List(o), "Answer not known.")
      * res0: EitherT[List, String, Int]  = List(Left(Answer not known.))
      * scala> EitherT.fromOptionF(List(Option(42)), "Answer not known.")
      * res1: EitherT[List, String, Int] = List(Right(42))
      * }}}
      */
    final def fromOptionF[F[_], E, A](fopt: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): EitherT[F, E, A] =
      F.map(fopt)(opt => Either.fromOption(opt, ifNone))

    /**  If the condition is satisfied, return the given `A` in `Right`
      *  lifted into the specified `Applicative`, otherwise, return the
      *  given `E` in `Left` lifted into the specified `Applicative`.
      *
      * {{{
      * scala> import cats.instances.nested.either._
      * scala> import cats.Id
      * scala> val userInput = "hello world"
      * scala> EitherT.cond[Id](
      *      |   userInput.forall(_.isDigit) && userInput.size == 10,
      *      |   userInput,
      *      |   "The input does not look like a phone number")
      * res0: EitherT[Id, String, String] = Left(The input does not look like a phone number)
      * }}}
      */
    final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied

    /**
      * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
      */
    final private[data] class CondPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
      def apply[E, A](test: Boolean, right: => A, left: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
        F.pure(Either.cond(test, right, left))
    }
  }
}
