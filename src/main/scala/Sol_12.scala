object Sol_12 extends App with SolInput {
  val allData = input("input12.txt")

  val init = allData.head
    .replace("initial state: ","")

  var part1Generations = 20

  val rules = allData.drop(2)
    .map(s => s.split(" => "))
    .map{case Array(from, to) => from -> to }
    .toMap.withDefaultValue(".")

  def calcSum(str: String, startIdx: Int = 0): Int =
    str.zipWithIndex.collect{case ('#', i) => i + startIdx}.sum

  def nextStep(str: String, idx: Int) = {
    val temp = ("..." + str + "...")
      .sliding(5)
      .map(rules(_))
      .mkString("")
    val newIdx = idx - 1 + temp.indexOf('#')
    val newStr = temp.dropWhile(_ == '.')
    (newStr, newIdx)
  }

  val iter1 = Iterator.iterate((init, 0)) (nextStep _ tupled)

  val (part1str, part1idx) = iter1.drop(part1Generations).next

  var res1 = calcSum(part1str, part1idx)
  println(res1)

  val part2Generations = 50000000000L

  val iter2 = Iterator.iterate((1, init, 0, "")){
    case (step, str, idx, _) =>
      val (nextStr, nextIdx) = nextStep(str, idx)
      (step + 1, nextStr, nextIdx, str)
  }

  val stable = iter2
    .dropWhile{case(_, cur, _, prev) => cur != prev}
    .map{case(step, cur, idx, _) => (step, idx, cur)}

  val (stableStep, stableIdx, stableStr) = stable.next
  val curSum = calcSum(stableStr, stableIdx)
  val diffscore = calcSum(stableStr, stableIdx + 1) - curSum

  val res2 = curSum + (part2Generations - stableStep + 1) * diffscore
  println(res2)
}