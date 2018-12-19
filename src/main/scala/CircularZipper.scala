case class CircularZipper[A](init: List[A], current: A, tail: List[A]) {
  def next: CircularZipper[A] = tail match {
    case hd :: tl => CircularZipper(current :: init, hd, tl)
    case Nil =>
      init.reverse match {
        case hd :: it => CircularZipper(List(current), hd, it)
        case Nil => this
      }
  }

  def prev: CircularZipper[A] = init match {
    case hd :: it => CircularZipper(it, hd, current :: tail)
    case Nil =>
      tail.reverse match {
        case hd :: tl => CircularZipper(tl, hd, List(current))
        case Nil => this
      }
  }

  def rotate(n: Int): CircularZipper[A] = {
    if (n == 0)
      this
    else if (n > 0)
      next.rotate(n - 1)
    else
      prev.rotate(n + 1)
  }

  def inserted(elem: A): CircularZipper[A] = CircularZipper(init, elem, current :: tail)

  def removed: (A, CircularZipper[A]) = tail match {
    case hd :: tl => (current, CircularZipper(init, hd, tl))
    case Nil =>
      val hd :: it = init
      (current, CircularZipper(it, hd, tail))
  }
}

object CircularZipper {
  def apply[A](elem: A): CircularZipper[A] = CircularZipper(Nil, elem, Nil)
}