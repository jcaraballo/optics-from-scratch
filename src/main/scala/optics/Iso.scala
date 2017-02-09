package optics

trait Iso[S, A] {
  def get:        S => A
  def reverseGet: A => S
}
