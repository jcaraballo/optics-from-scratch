package optics


trait Prism[S, A] {
  def getOption: S => Option[A]
  def reverseGet: A => S
}

object Prism {
  def apply[S, A](getOptionFunction: S => Option[A])(reverseGetFunction: A => S): Prism[S, A] =
    new Prism[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val reverseGet: A => S        = reverseGetFunction
    }
}
