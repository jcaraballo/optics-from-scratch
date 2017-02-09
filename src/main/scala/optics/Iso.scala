package optics

trait Iso[S, A] {
  def get:        S => A
  def reverseGet: A => S

  def modify(f: A => A): S => S = get andThen f andThen reverseGet
  def set(a: A): S => S = modify(_ => a)

  def compose[B](other: Iso[A, B]): Iso[S, B] =
    new Iso[S, B] {
      override def get: (S) => B = Iso.this.get andThen other.get
      override def reverseGet: (B) => S = other.reverseGet andThen Iso.this.reverseGet
    }
}
