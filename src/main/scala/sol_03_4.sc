val claims = List("#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2")
val parsedClaims = claims
  .map(s => """\d+""".r.findAllIn(s)
    .map(_.toInt).toList)
  .map{ case List(id, x, y, h, w) => (id, (x,h), (y, w))}
val claimCoords = parsedClaims
  .map{case (id, (x,h),(y,w)) => id -> Range(x, x+h)
    .flatMap(x => Range(y, y+w).map(y => (x,y)))}

val allCoords = claimCoords.flatMap{ case (_,coords) => coords }

val intersected = allCoords
  .groupBy(identity)
  .filter{ case (_, area) => area.length > 1 }
  .keys.toSet
val c3 = claimCoords.last

val ic = claimCoords
  .map{case (id, cells) => (id, cells.forall(coord => !intersected.contains(coord)))}

val fc = claimCoords
  .filter{case (_, cells) => cells.forall(coord => !intersected.contains(coord))}
  .map(_._1).head