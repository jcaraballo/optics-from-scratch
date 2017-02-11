package optics

trait Lens[S, A] {
  def get: S => A
  def set: (A, S) => S
}

object Lens {
  def apply[S, A](getter: S => A)(setter: (A, S) => S): Lens[S, A] =
    new Lens[S, A] {
      override val set: (A, S) => S = setter
      override val get: (S) => A = getter
    }
}
