case class Claim(id: Int, x: Int, y: Int, h: Int, w: Int)
{
  def allCoords: Seq[(Int, Int)] = Range(x,x+h).flatMap(x => Range(y,y+w).map(y => (x, y)))
}
case object Claim
{
  def Parse(str: String): Claim = {
    val re = """#(?<id>\d+) @ (?<x>\d+),(?<y>\d+): (?<h>\d+)x(?<w>\d+)""".r
    val re(id, x, y, h, w) = str
    new Claim(id.toInt, x.toInt, y.toInt, h.toInt, w.toInt)
  }
}

val s1 = "#1242 @ 900,330: 20x15"
val s2 = "#1243 @ 910,335: 15x15"

val r1 = Claim.Parse(s1)
val r2 = Claim.Parse(s2)

val rx1 = Range(r1.x, r1.x + r1.h)
val ry1 = Range(r1.y, r1.y + r1.w)
val rx2 = Range(r2.x, r2.x + r2.h)
val ry2 = Range(r2.y, r2.y + r2.w)

val mc1 = rx1.flatMap(x => ry1.map(y => (x,y)))
val mc2 = rx2.flatMap(x => ry2.map(y => (x,y)))

mc1.length
mc2.length

val lr = List(r1, r2)

mc1.intersect(mc2).length

mc1.union(mc2).groupBy(identity).filter{case (_, kl) => kl.length > 1}.count(_ => true)
val res = lr.map(r => r.allCoords).fold(Seq.empty){case (acc, cc) => acc.union(cc)}
res.groupBy(identity).filter{case (_, kl) => kl.length > 1}.count(_ => true)