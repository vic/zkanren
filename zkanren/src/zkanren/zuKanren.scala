package zkanren

import zio.*
import zio.stm.*
import zio.stream.*

object zuKanren {

  type Term[+X] = Var[X] | Value[X]

  type Value[+X] = X

  trait Var[+X]
  object Var {
    def unapply[X](t: Term[X]): Option[Var[X]] = t match {
      case v: Var[X] => Some(v)
      case _         => None
    }
  }

  // Unification step. Either unifies into A or fails with both terms.
  type Step[+X, +A] = ZSTM[SMap, (Term[X], Term[X]), A]

  trait Unify[X] {
    def apply(a: Term[X], b: Term[X]): Step[X, SMap]

    final def orElse(y: => Unify[X]): Unify[X] = { (a, b) =>
      apply(a, b).orElse(y(a, b))
    }
  }

  object Unify {
    def apply[X](fn: (Term[X], Term[X]) => Step[X, SMap]): Unify[X] = {
      (a, b) => fn(a, b)
    }

    def fail[X]: Unify[X] = Unify[X] { (a, b) => ZSTM.fail(a -> b) }
    def same[X]: Unify[X] = ???
    def bind[X]: Unify[X] = ???
    def reduce[X](u: => Unify[X]): Unify[X] = ???

    def default[X]: Unify[X] = same.orElse(bind).orElse(reduce(default))

  }

  object SMap {

    private def walk[X](
        b: TMap[Var[X], Term[X]],
        t: Term[X]
    ): USTM[Term[X]] =
      t match {
        case Var(v) =>
          b.get(v).flatMap {
            case None         => STM.succeed(v)
            case Some(Var(v)) => walk(b, v)
            case Some(term)   => STM.succeed(term)
          }
        case x => STM.succeed(x)
      }

    def isSame[X]: Unify[X] = { (a: Term[X], b: Term[X]) =>
      ZSTM.serviceWithSTM[SMap] { smap =>
        val bindings = smap.bindings.asInstanceOf[Bindings[X]]
        (walk(bindings, a) zip walk(bindings, b)).flatMap {
          case (x, y) if x == y => STM.succeed(smap)
          case (x, y)           => STM.fail(a -> b)
        }
      }
    }

  }

  type Bindings[T] = TMap[Var[T], Term[T]]

  case class SMap(
      unify: Unify[Any],
      nextVar: TRef[Var[Any]],
      bindings: Bindings[Any]
  )

}
