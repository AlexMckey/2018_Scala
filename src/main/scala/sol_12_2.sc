val initStr = "initial state: #..#.#..##......###...###"
val init = initStr.replace("initial state: ","")
val rulesStr = "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #".lines
val rules = rulesStr
  .map(s => s.split(" => "))
  .map{case Array(from, to) => from -> to }
  .toMap.withDefaultValue(".")

def calcSum(str: String, startIdx: Int = 0): Int =
  str.zipWithIndex.collect{case ('#', i) => i + startIdx}.sum

val res = Range(1, 20).foldLeft((init, 0)){
  case ((str, idx), _) =>
    val temp = ("..." + str + "...")
      .sliding(5)
      .map(pat => rules(pat))
    val newIdx = idx - 1 + temp.indexOf('#')
    val newStr = temp.dropWhile(_ == ".")
    (newStr.mkString, newIdx)
}

val resSum = calcSum _ tupled res