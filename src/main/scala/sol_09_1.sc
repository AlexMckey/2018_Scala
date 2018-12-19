val input = "9 players; last marble is worth 25 points"
val marbleInput = """\d+""".r.findAllIn(input).map(_.toInt).toList
val List(elvesCnt, endMarble) = marbleInput
import scala.collection.JavaConverters._
val marbleCircle = new java.util.LinkedList[Int]()

implicit class TotateList[T](val lst: java.util.LinkedList[Int])
  extends AnyVal {
  def rotate(cnt: Int): Unit = {
    if (cnt > 0)
      for (_ <- 0 until cnt) {
        lst.addFirst(lst.removeLast())
      }
    else
      for (_ <- 0 until (-1 - cnt)) {
        lst.addLast(lst.removeFirst())
      }
  }
}
marbleCircle.addLast(0)
marbleCircle.rotate(-2)
marbleCircle.addLast(1)
marbleCircle.rotate(-2)
marbleCircle.addLast(2)
marbleCircle.rotate(-2)
marbleCircle.addLast(3)
marbleCircle.rotate(-2)
marbleCircle.addLast(4)
marbleCircle.rotate(-2)
marbleCircle.addLast(5)
marbleCircle.rotate(-2)
marbleCircle.addLast(6)
marbleCircle.toArray