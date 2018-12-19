case class Game(elvesCnt: Int, goalMarble: Int)
{
  val marbleCircle = new java.util.LinkedList[Int]()
  val scores = new Array[Long](elvesCnt)

  def rotate(cnt: Int): Unit = {
    if (cnt > 0)
      for (_ <- 0 until cnt) {
        marbleCircle.addFirst(marbleCircle.removeLast())
      }
    else
      for (_ <- 0 until (-1 - cnt)) {
        marbleCircle.addLast(marbleCircle.removeFirst())
      }
  }

  def CalcResult: Long = {
    marbleCircle.addLast(0)
    for (marble <- 1 to goalMarble) {
      if (marble % 23 == 0) {
        rotate(-7)
        scores(marble % elvesCnt) += marbleCircle.pop() + marble

      } else {
        rotate(2)
        marbleCircle.addLast(marble)
      }
    }
    scores.max
  }
}

object Sol_09 extends App with SolInput {
  val marbleInput = """\d+""".r
    .findAllIn(input("input09.txt").head)
      .map(_.toInt)
  val List(elvesCnt, endMarble) = marbleInput.toList

  val res1 = Game(elvesCnt, endMarble).CalcResult

  println(s"$res1") // 424639

  val res2 = Game(elvesCnt, endMarble * 100).CalcResult

  println(s"$res2") // 3516007333
}