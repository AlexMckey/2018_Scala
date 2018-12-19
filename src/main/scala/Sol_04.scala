object Sol_04 extends App with SolInput  {
  val guardsLog = input("input04.txt")
  //val regexp = """\[(?<date>[\d-: ]+)] (?:(?<awake>wakes up)|(?<asleep>falls asleep)|(?:Guard #(?<id>\d+) begins shift))""".r
  //val events = guardsLog.map(s => regexp.findAllMatchIn(s))
  val guardsLogSorted = guardsLog.sorted
  import scala.collection.JavaConverters._
  val marbleCircle = new java.util.LinkedList[Int]().asScala

}
