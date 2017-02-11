package optics


trait Prism[S, A] {
  def getOption: S => Option[A]
  def reverseGet: A => S

  def modify(f: A => A): S => S = s => getOption(s).map(f andThen reverseGet).getOrElse(s)
  def set(a: A): S => S = modify(_ => a)
}

object Prism {
  def apply[S, A](getOptionFunction: S => Option[A])(reverseGetFunction: A => S): Prism[S, A] =
    new Prism[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val reverseGet: A => S        = reverseGetFunction
    }
}
