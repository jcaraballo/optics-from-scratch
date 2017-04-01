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
}
