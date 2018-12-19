object Sol_01 extends App with SolInput {

  val freqChanges = input("input01.txt").map(_.toInt)

  val res1 = freqChanges.sum

  println(s"$res1")

  implicit class CycleSeqs[T](val seq: Stream[T]) extends AnyVal {
    //def cycle: Stream[T] = Stream.continually(seq).flatten
    //def cycle[T](seq: Seq[T]) = Stream.from(0).flatten(_ => seq)
    //def repeatedSeq(idx: Int, lst:Seq[Int]): Stream[Int] = Stream.cons(lst(idx), repeatedSeq((idx + 1)%lst.length, lst))
    def  cycle: Stream[T] = seq #::: seq
  }

  val res2 = freqChanges.cycle
    .scanLeft((0, Set.empty[Int], false))
    {case ((curFreq, set, _), freqDiff) =>
      val newFreq = curFreq + freqDiff
      val check = set.contains(newFreq)
      (newFreq, set + newFreq, check)
    }.dropWhile(!_._3).head._1

  println(s"$res2")
}
