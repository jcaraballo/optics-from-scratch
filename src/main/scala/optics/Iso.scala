package optics

trait Iso[S, A] {
  def get:        S => A
  def reverseGet: A => S

  def modify(f: A => A): S => S = get andThen f andThen reverseGet
  def set(a: A): S => S = modify(_ => a)

  def compose[B](other: Iso[A, B]): Iso[S, B] =
    Iso[S, B](this.get andThen other.get)(other.reverseGet andThen this.reverseGet)
  def compose[B](other: Prism[A, B]): Prism[S, B] =
    Prism[S, B](this.get andThen other.getOption)(other.reverseGet andThen this.reverseGet)
  def compose[B](other: Lens[A, B]): Lens[S, B] =
    Lens[S, B](this.get andThen other.get){ (b, s) =>
      this.reverseGet(other.set(b, this.get(s)))
    }
}

object Iso {
  def apply[S, A](getFunction: S => A)(reverseGetFunction: A => S): Iso[S, A] =
    new Iso[S, A] {
      override def get: S => A = getFunction
      override def reverseGet: A => S = reverseGetFunction
    }
}
