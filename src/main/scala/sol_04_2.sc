val testLog ="""
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""

val evr = """\[[\d-]+ \d\d:(?<min>\d\d)] (?:G.+ #(?<id>\d+).+|f.+|w.+)""".r

val evs = testLog
  .trim()
  .split('\n')
  .sorted
  .map(s => evr
    .findAllIn(s)
    .subgroups
    .map(s => if (s == null) 0 else s.toInt))
  .toList

val id = evs.take(1).head(1)
val evs1 = evs
  .drop(1)
  .takeWhile(lst => lst.tail.head == 0)
  .sliding(2,2)
  .map{ case List(sleep, wake)
    => Range(sleep.head, wake.head)}
  .toList