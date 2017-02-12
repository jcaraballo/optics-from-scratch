package optics


trait Prism[S, A] {
  def getOption: S => Option[A]
  def reverseGet: A => S

  def modifyOption(f: A => A): S => Option[S] = s => getOption(s).map(f andThen reverseGet)
  def modify(f: A => A): S => S = s => modifyOption(f)(s).getOrElse(s)
  def set(a: A): S => S = modify(_ => a)
}

object Prism {
  def apply[S, A](getOptionFunction: S => Option[A])(reverseGetFunction: A => S): Prism[S, A] =
    new Prism[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val reverseGet: A => S        = reverseGetFunction
    }
}
