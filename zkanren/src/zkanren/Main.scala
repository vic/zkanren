package zkanren

import zio.*
import zio.stm.*
import zio.stream.*

object Foo {
  import zuKanren.*

  def main(args: Array[String]): Unit = {
    println(s"JA ${Var.zero[Int]}")
    println(s"JA ${Var.zero[Int]}")
  }

}

/*
object Foo extends scala.App {

  trait LTerm {}

  // Resolved term (aka, walked)
  type RTerm = LTerm

  trait LVar extends LTerm {}
  object LVar:
    def zero: LVar = ???

  trait Zip

  type EStream[-R, +E, +A] = ZStream[R, Nothing, Either[E, A]]

  // This is a Merge stream when trying to unify two terms in a substitution-map.
  // If successful, unifies to X, otherwise fails with both terms.
  type MStream[+X] = ZStream[SMap, Nothing, Either[(RTerm, RTerm), X]]

  trait Unify {
    def apply(a: LTerm, b: LTerm): MStream[SMap]

    final def orElse(y: => Unify): Unify = (a: LTerm, b: LTerm) =>
      apply(a, b).flatMap {
        case Right(value) => ZStream.succeed(Right(value))
        case Left((a, b)) => y(a, b)
      }
  }

  object Unify {
    def apply[X](
        cond: (LTerm, LTerm) => MStream[X]
    )(make: X => MStream[SMap]): Unify = { (a: LTerm, b: LTerm) =>
      cond(a, b).flatMap {
        case Right(x)     => make(x)
        case Left((a, b)) => fail(a, b)
      }
    }

    val fail: Unify = { (a: LTerm, b: LTerm) =>
      ZStream.succeed(Left(a -> b))
    }

    val same: Unify =
      Unify(SMap.isSame)(_ => SMap.service.map(Right(_)))

    val bind: Unify = Unify(SMap.isBindable)(SMap.bind)

    def reduce(u: => Unify): Unify =
      Unify(SMap.isReducible)(SMap.reduce(u.apply))

    val default: Unify =
      Unify.same
        .orElse(Unify.bind)
        .orElse(Unify.reduce(default))
  }

  object SMap {
    def service: ZStream[SMap, Nothing, SMap] = ZStream.service[SMap]

    def get(a: LTerm): ZStream[SMap, Nothing, RTerm] =
      ???

    def isSame(
        a: RTerm,
        b: RTerm
    ): EStream[SMap, (RTerm, RTerm), (RTerm, RTerm)] =
      (get(a) zip get(b)).map {
        case (x, y) if x == y => Right(x -> y)
        case (x, y)           => Left(x -> y)
      }

    def isBindable(
        a: RTerm,
        b: RTerm
    ): EStream[SMap, (RTerm, RTerm), (LVar, RTerm)] = {
      ???
    }

    def bind(a: LVar, b: RTerm): EStream[SMap, Nothing, SMap] = {
      ???
    }

    def isReducible(a: RTerm, b: RTerm): EStream[SMap, (RTerm, RTerm), Zip] = {
      ???
    }

    def reduce[R, E, A](
        f: (RTerm, RTerm) => EStream[R, E, A]
    )(zip: Zip): EStream[R, E, A] = {
      ???
    }

    def unify(a: LTerm, b: LTerm): MStream[SMap] =
      service.flatMap(_.unify(a, b))

    def empty: USTM[SMap] = for {
      nextVar <- TRef.make[LVar](???)
      bindings <- TMap.empty[LVar, LTerm]
    } yield SMap(
      unify = Unify.default,
      nextVar = nextVar,
      bindings = bindings
    )

  }

  // The substitution map is the *state* where we bind logic variables to
  // any other term, either values or other lvars.
  case class SMap(
      unify: Unify,
      nextVar: TRef[LVar],
      bindings: TMap[LVar, LTerm]
  )

}
**/
