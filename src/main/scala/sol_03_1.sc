val s1 = "#1242 @ 900,330: 20x15"
val s2 = "#1243 @ 910,335: 15x15"
val re = """#(?<id>\d+) @ (?<x>\d+),(?<y>\d+): (?<h>\d+)x(?<w>\d+)""".r
re.getClass
val re(t1, t2, t3, t4, t5) = s1
"""\d+""".r.findAllIn(s1).toList
val ms = re.findAllIn(s1)

val m1 = ms.subgroups
val m2 = ms.group("id")
