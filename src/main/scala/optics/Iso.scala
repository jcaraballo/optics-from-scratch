package optics

trait Iso[S, A] {
  def get:        S => A
  def reverseGet: A => S

  def modify(f: A => A): S => S = get andThen f andThen reverseGet
  def set(a: A): S => S = modify(_ => a)

  def compose[B](other: Iso[A, B]): Iso[S, B] =
    Iso[S, B](this.get andThen other.get)(other.reverseGet andThen this.reverseGet)
}

object Iso {
  def apply[S, A](getFunction: S => A)(reverseGetFunction: A => S): Iso[S, A] =
    new Iso[S, A] {
      override def get: S => A = getFunction
      override def reverseGet: A => S = reverseGetFunction
    }
}
