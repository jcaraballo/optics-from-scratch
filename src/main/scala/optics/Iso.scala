package optics

trait Iso[S, A] {
  def get:        S => A
  def reverseGet: A => S

  def modify(f: A => A): S => S = get andThen f andThen reverseGet
}
