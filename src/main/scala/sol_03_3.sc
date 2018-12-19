val claims = List("#1242 @ 900,330: 20x15","#1243 @ 910,335: 15x15")
//val regexp = """#(?<id>\d+) @ (?<x>\d+),(?<y>\d+): (?<h>\d+)x(?<w>\d+)""".r
val parsedClaims = claims
  .map(s => """\d+""".r.findAllIn(s)
    .map(_.toInt).toList.tail)
  .map(lst => ((lst.head, lst.head + lst.tail.tail.head),
               (lst.tail.head, lst.tail.head + lst.last)))
val coords = parsedClaims
  .flatMap{case ((x,xh),(y,yw)) => Range(x, xh)
    .flatMap(x => Range(y, yw).map(y => (x,y)))}

val intersected = coords
  .groupBy(identity)
  .map{ case (k, vals) => (k, vals.length)}
  .filter(_._2 > 1)

val cnt = intersected.count(_ => true)