package optics


trait Prism[S, A] {
  def getOption: S => Option[A]
  def reverseGet: A => S

  def modifyOption(f: A => A): S => Option[S] = s => getOption(s).map(f andThen reverseGet)
  def modify(f: A => A): S => S = s => modifyOption(f)(s).getOrElse(s)
  def set(a: A): S => S = modify(_ => a)

  def compose[B](other: Prism[A, B]): Prism[S, B] =
    Prism[S, B](s => this.getOption(s).flatMap(other.getOption))(other.reverseGet andThen this.reverseGet)
  def compose[B](other: Iso[A, B]): Prism[S, B] =
    Prism[S, B](s => getOption(s).map(other.get))(other.reverseGet andThen this.reverseGet)
  def compose[B](other: Lens[A, B]): Optional[S, B] =
    Optional[S, B](s => this.getOption(s).map(other.get)){(b, s) =>
      this.getOption(s).map(other.set(b, _)).map(this.reverseGet).getOrElse(s)
    }
}

object Prism {
  def apply[S, A](getOptionFunction: S => Option[A])(reverseGetFunction: A => S): Prism[S, A] =
    new Prism[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val reverseGet: A => S        = reverseGetFunction
    }
}
