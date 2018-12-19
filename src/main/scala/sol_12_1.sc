val initStr = "initial state: #..#.#..##......###...###"
var idx = 0
val init = initStr.replace("initial state: ","")

val rulesStr = "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #".lines

val rules = rulesStr
  .map(s => s.split(" => "))
  .map{case Array(from, to) => from -> to }
  .toMap.withDefaultValue(".")

rules("...")

val sources = ("..." + init + "...")
  .sliding(5).toList

val newRes = sources
  .map(pat => rules(pat))
  .mkString("")

if (newRes.head == '#') idx += 1
idx = 2

val res1 = newRes
  .zipWithIndex
  .filter{case (ch, _) => ch == '#'}
  .map{case (_, i) => i - idx}
  .sum

val res2 = newRes
  .zipWithIndex
  .collect{case ('#', i) => i - idx}
  .sum

val s = "..sf..."
s.dropWhile(_ == '.')

