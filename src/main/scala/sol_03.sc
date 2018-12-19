case class Claim(id: Int, x: Int, y: Int, h: Int, w: Int)

val s1 = "#1242 @ 900,330: 20x15"
val re = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
val re(t1, t2, t3, t4, t5) = s1

t1
t2
t3
t4
t5