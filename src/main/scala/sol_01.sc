import scala.collection.immutable.HashSet
import scala.io.Source.fromFile

val s1 = "+ 5"
val s2 = "- 3"

s1.replace(" ", "").toInt
s2.replace(" ", "").toInt

val it = Iterator(1, 2, 3, 4, 5)

implicit class CycleSeqs[T](val seq: Seq[T]) extends AnyVal {
  def cycle = Stream.continually(seq).flatten
}

val nums = it.toSeq.cycle.take(11).toList

val freqChanges = fromFile("D:\\DevsExercises\\AdventOfCode\\AdventOfCode_2018-master\\2018_Scala\\src\\main\\scala\\input01.txt")
  .getLines().map(_.toInt)

val l1 = List(1, -2, 3, 1)
val l2 = List(3, 3, 4, -2, -4)

val l = freqChanges.toSeq

println(l.length)

val res2 = l.cycle
  .scanLeft((0, Set.empty[Int], false)) ((acc, v) => {
    val newFreq = acc._1 + v
    val check = acc._2.contains(newFreq)
    (newFreq, acc._2 + newFreq, check)
  }).dropWhile(!_._3).head._1

val res3 = l.cycle
  .scanLeft((0, Set.empty[Int], false)) ((acc, v) => {
    val newFreq = acc._1 + v
    val check = acc._2.contains(newFreq)
    (newFreq, acc._2 + newFreq, check)
  }).take(10).toList

val x = new { val count = 5}
x.count