package cats.instances.nested

import cats.{Functor, Invariant, Monad, Contravariant, ~>, Show}
import cats.kernel.{Eq, Order, PartialOrder}
import cats.instances.nested.either.EitherT

object option {

  type OptionT[F[_], A] = F[Option[A]]

  implicit final class NestedOptionOps[F[_], A](val value: F[Option[A]]) extends AnyVal {

    def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] =
      F.map(value)(_.fold(default)(f))

    /**
      * Catamorphism on the Option. This is identical to [[fold]], but it only has
      * one parameter list, which can result in better type inference in some
      * contexts.
      */
    def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
      fold(default)(f)

    def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
      F.map(value)(_.map(f))

    def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[F]): OptionT[F, B] =
      F.imap(value)(_.map(f))(_.map(g))

    def contramap[B](f: B => A)(implicit F: Contravariant[F]): OptionT[F, B] =
      F.contramap(value)(_.map(f))

    /**
      * Modify the context `F` using transformation `f`.
      */
    def mapK[G[_]](f: F ~> G): OptionT[G, A] = f(value)

    def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] =
      F.flatMap(value){
        case Some(a) => F.map(f(a))(Some(_))
        case None    => F.pure(None)
      }

    def mapFilter[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
      transform(_.flatMap(f))

    def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
      F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f))

    def flatTransform[B](f: Option[A] => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
      F.flatMap(value)(f)

    def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
      F.map(value)(f)

    def getOrElse[B >: A](default: => B)(implicit F: Functor[F]): F[B] =
      F.map(value)(_.getOrElse(default))

    def getOrElseF[B >: A](default: => F[B])(implicit F: Monad[F]): F[B] =
      F.flatMap(value)(_.fold(default)(F.pure))

    def collect[B](f: PartialFunction[A, B])(implicit F: Functor[F]): OptionT[F, B] =
      F.map(value)(_.collect(f))

    def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(value)(_.exists(f))

    def filter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
      F.map(value)(_.filter(p))

    def withFilter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
      filter(p)(F)

    def filterNot(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
      F.map(value)(_.filterNot(p))

    def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(value)(_.forall(f))

    def isDefined(implicit F: Functor[F]): F[Boolean] =
      F.map(value)(_.isDefined)

    def isEmpty(implicit F: Functor[F]): F[Boolean] =
      F.map(value)(_.isEmpty)

    def orElseF(default: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
      F.flatMap(value) {
        case s @ Some(_) => F.pure(s)
        case None        => default
      }

//    def toRight[L](left: => L)(implicit F: Functor[F]): EitherT[F, L, A] =
//      EitherT(cata(Left(left), Right.apply))
//
//    def toLeft[R](right: => R)(implicit F: Functor[F]): EitherT[F, A, R] =
//      EitherT(cata(Right(right), Left.apply))

    def show(implicit F: Show[F[Option[A]]]): String = F.show(value)

    def compare(that: OptionT[F, A])(implicit o: Order[OptionT[F, A]]): Int =
      o.compare(value, that)

    def partialCompare(that: OptionT[F, A])(implicit p: PartialOrder[OptionT[F, A]]): Double =
      p.partialCompare(value, that)

    def ===(that: OptionT[F, A])(implicit eq: Eq[F[Option[A]]]): Boolean =
      eq.eqv(value, that)

//    def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] =
//      G.map(F.compose(optionInstance).traverse(value)(f))(OptionT.apply)
//
//    def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
//      F.compose(optionInstance).foldLeft(value, b)(f)
//
//    def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
//      F.compose(optionInstance).foldRight(value, lb)(f)

  }

}