object Median {
  def NLogNMedian(lst: Seq[Int]): Int = {
    val lSorted = lst.sorted
    val halfLen = lSorted.length / 2
    if (lSorted.length % 2 == 1)
      lSorted(halfLen)
    else
      lSorted.slice(halfLen - 1, halfLen + 1).sum / 2
  }

  def QuickSelectMedian(lst: Seq[Int], pivot_fn: Seq[Int] => Int): Int = {
    val len = lst.length
    val halfLen = len / 2
    if (len % 2 == 1)
      QuickSelect(lst, halfLen, pivot_fn)
    else
      (QuickSelect(lst, halfLen - 1, pivot_fn) + QuickSelect(lst, halfLen, pivot_fn)) / 2
  }

  def QuickSelect(lst: Seq[Int], k: Int, pivot_fn: Seq[Int] => Int): Int = {
    if (lst.length == 1) {
      assert(k == 0)
      lst.head
    } else {
      val pivot = pivot_fn(lst)
      val (less, right) = lst.partition(_ < pivot)
      val (highs, pivots) = right.partition(_ > pivot)
      if (k < less.length)
        QuickSelect(less, k, pivot_fn)
      else if (k < less.length + pivots.length)
        pivots.head
      else
        QuickSelect(highs, k - less.length - pivots.length, pivot_fn)
    }
  }

  def RandomChoice(lst: Seq[Int]): Int = {
    val r = scala.util.Random
    lst(r.nextInt(lst.length))
  }

  def PickPivot(lst: Seq[Int]): Int = {
    require(lst.nonEmpty)
    if (lst.length < 5)
      NLogNMedian(lst)
    else {
      val chunks = lst.grouped(5).filter(ch => ch.length == 5)
      val sortedChunks = chunks.map(_.sorted)
      val medians = sortedChunks.map(ch => ch(2)).toSeq
      QuickSelectMedian(medians, PickPivot)
    }
  }

  implicit class MedianSeqs(val seq: Seq[Int]) extends AnyVal {
      def  median: Int = Median.QuickSelectMedian(seq, Median.PickPivot)
  }
}
