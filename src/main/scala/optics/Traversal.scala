package optics

import scala.collection.mutable.ArrayBuffer

trait Traversal[S, A] {
  def modify(f: A => A): S => S

  def getAll: S => List[A] = { s =>
    val as = ArrayBuffer[A]()
    modify{a =>
      as.append(a)
      a
    }(s)
    as.toList
  }

  def set: (A, S) => S = (a, s) => modify(_ => a)(s)

  def headOption: S => Option[A] = getAll andThen (_.headOption)
}

object Traversal {
  def apply2[S, A](get1: S => A, get2: S => A)(sett: (A, A, S) => S): Traversal[S, A] = (f: A => A) => { s => sett(f(get1(s)), f(get2(s)), s) }

  // Warning: Apparently this can be unsafe for lenses modifying the same A. See https://github.com/julien-truffaut/Monocle/issues/379#issuecomment-236374838
  def unsafeFromLenses[S, A](lenses: List[Lens[S, A]]): Traversal[S, A] = (f: A => A) => { s =>
    lenses.foldLeft(s)((s, l) => l.modify(f)(s))
  }
}
