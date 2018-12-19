import java.time._
import java.time.format.DateTimeFormatter

val testLog = """[1518-11-01 00:00] Guard #10 begins shift
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

val evr = """\[[\d-]+ \d\d:(?<min>\d\d)] (?:(?<awake>wakes up)|(?<asleep>falls asleep)|(?:Guard #(?<id>\d+) begins shift))""".r

import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

private val DATE_FORMAT = new DateTimeFormatterBuilder()
  .appendPattern("yyyy-MM-dd HH:mm")
  .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
  .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
  .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
  .toFormatter

val evs = testLog
  .trim()
  .split('\n')
  .map(s => evr.findAllIn(s).subgroups)
  .map{ case List(dts, sleeps, wakes, ids) =>
    (LocalDateTime.parse(dts, DATE_FORMAT),
      sleeps != null,
      wakes != null,
      if (ids != null) ids.toInt else 0)
  }
  .sortBy{case (dt, _, _, _) => dt} (_.compareTo(_))