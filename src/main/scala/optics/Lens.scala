package optics

trait Lens[S, A] {
  def get: S => A
  def set: (A, S) => S

  def modify(f: A => A): S => S = s => set(f(get(s)), s)

  def compose[B](other: Lens[A, B]): Lens[S, B] =
    Lens[S, B](this.get andThen other.get){(b, s) =>
      val oldA = this.get(s)
      val newA = other.set(b, oldA)
      this.set(newA, s)
    }
}

object Lens {
  def apply[S, A](getter: S => A)(setter: (A, S) => S): Lens[S, A] =
    new Lens[S, A] {
      override val set: (A, S) => S = setter
      override val get: (S) => A = getter
    }
}
