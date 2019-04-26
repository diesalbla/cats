package cats

/**
  * A FunTo is an object that can take an input of type A and returns an output of type B. 
  * This definition is similar to that of `scala.Function1`, but it actually departs from it in 
  * several aspects: 
  * 
  * - Variance: the `scala.Function` is contravariant in its first parameter and invariant on the second one. 
  *   Whereas this type is invariant in both the input and output types. 
  * - API: the `scala.Function` defines several methods, like `compose` and `andThen`, that sometimes 
  *   collide with those from the instances we want to build in Cats.
  * - PartialFunctions: in the Scala library, the trait `PartialFunction` extends from `Function`, which means 
  *   that a `PartialFunction` can always be passed in places where a _total_ function would be expected. 
  *   This has caused headaches for maintainers of some libraries.
  * 
  */
abstract class FunTo[A, B] {
  def apply(A: A): B
}
