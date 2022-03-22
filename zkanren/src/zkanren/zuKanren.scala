package zkanren

import zio.*
import zio.stm.*
import zio.stream.*

import scala.compiletime.ops.int

// Need a fast: occurs? term.dependsOn(var)?
// Terms are streams
//
// Unifying a two streams of terms, if one ends before the other, dont unify.

// http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
// https://mullr.github.io/micrologic/literate.html
// https://kwannoel.github.io/uKanren/index.html#/extensions
object zuKanren {

  type Term[+X] = Var[X] | Value[X]

  opaque type Value[+X] = UStream[X]
  object Value {}

  // TODO: Use TRef[LongMap[Term[T]]
  type Bindings[T] = TMap[Var[T], Term[T]]

  sealed trait Var[+X] {
    type T
    type N <: Int

    val tTag: Tag[T]
    val nTag: Tag[N]
    def n: Int = nTag.tag.shortName.toInt
    override def toString: String = s"$$${n}:${tTag.tag.repr}"
  }

  object Var {

    lazy val x: izumi.reflect.Tag[Int] = ???

    def apply[X]: PartialNext[X] = new PartialNext[X]
    class PartialNext[X](private val dummy: Unit = ()) extends AnyVal {
      def apply[M <: Int](
          v: Var[Any]
      )(implicit ev: Tag[X], m: M =:= int.S[v.N], ev0: Tag[M]): Var[X] =
        new Var[X] {
          override type T = X
          override type N = M
          override val tTag = summon[Tag[T]]
          override val nTag = summon[Tag[N]]
//          override val n: N = m.asInstanceOf[N]
        }
    }

    private type AUX[X, M] = Var[X] { type T = X; type M = N }

    def n(v: Var[Any])(implicit ev: v.N): String = ev.toString
    def tag(v: Var[Any])(implicit ev: Tag[v.T]): Tag[v.T] = ev

//    def toString(v: Var[Any]): String = s"Var[${n(v)}:${tag(v)}]"

    def zero[X: Tag]: Var[X] = new Var[X] {
      override type T = X
      override type N = 0
      override val tTag = summon[Tag[T]]
      override val nTag = summon[Tag[N]]
      //      override val n: N = 0
    }

    def unapply[X](t: Term[X]): Option[Var[X]] = t match {
      case v: Var[X @unchecked] => Some(v)
      case _                    => None
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
    def same[X]: Unify[X] = SMap.unifySame[X]
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

    def unifySame[X]: Unify[X] = { (a: Term[X], b: Term[X]) =>
      ZSTM.serviceWithSTM[SMap] { smap =>
        val bindings = smap.bindings.asInstanceOf[Bindings[X]]
        (walk(bindings, a) zip walk(bindings, b)).flatMap {
          case (x, y) if x == y => STM.succeed(smap)
          case (x, y)           => STM.fail(a -> b)
        }
      }
    }

//    def unifyBind[X]: Unify[X] = { (a: Term[X], b: Term[X]) =>
//      ZSTM.serviceWithSTM[SMap] { smap =>
//        val bindings = smap.bindings.asInstanceOf[Bindings[X]]
//
//      }
//    }

  }

  case class SMap(
      unify: Unify[Any],
      nextVar: TRef[Var[Any]],
      bindings: Bindings[Any]
  )

}
