package optics

trait Optional[S, A] {
  def getOption: S => Option[A]
  def set:  (A, S) => S
}

object Optional {
  def apply[S, A](getOptionFunction: S => Option[A])(setter:  (A, S) => S): Optional[S, A] =
    new Optional[S, A] {
      override val getOption: S => Option[A] = getOptionFunction
      override val set:  (A, S) => S = setter
    }
}
