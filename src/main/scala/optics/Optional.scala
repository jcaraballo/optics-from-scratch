package optics

trait Optional[S, A] {
  def getOption: S => Option[A]
  def set:  (A, S) => S

  def modifyOption(f: A => A): S => Option[S] = s => getOption(s).map(a => set(f(a), s))
}

object Optional {
  def apply[S, A](getOptionFunction: S => Option[A])(setter:  (A, S) => S): Optional[S, A] =
    new Optional[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val set:  (A, S) => S = setter
    }
}
