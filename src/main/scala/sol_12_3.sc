val initStr = "initial state: #..#.#..##......###...###"
val init = initStr.replace("initial state: ","")
val rulesStr = "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #".lines
val rules = rulesStr
  .map(s => s.split(" => "))
  .map{case Array(from, to) => from -> to }
  .toMap.withDefaultValue(".")

def calcSum(str: String, startIdx: Int = 0): Int =
  str.zipWithIndex.collect{case ('#', i) => i + startIdx}.sum

val res = Iterator.iterate((1, init, 0, "")){
  case (_step, str, _idx, _) =>
    val temp = ("..." + str + "...")
      .sliding(5)
      .map(pat => rules(pat)).mkString
    val newIdx = _idx - 1 + temp.indexOf('#')
    val newStr = temp.dropWhile(_ == '.') //.mkString
    (_step + 1, newStr, newIdx, str)
}

val iter = res
  .dropWhile{case(_, cur, _, prev) => cur != prev}
  .map{case(_step, cur, _idx, _) => (_step, _idx, cur)}
val (step, idx, resRep) = iter.next
val score1 = calcSum(resRep, idx)
val (nstep, nidx, nres) = iter.next
val score2 = calcSum(nres, nidx)
val (nnstep, nnidx, nnres) = iter.next
val score3 = calcSum(nnres, nnidx)