object Sol_03 extends App with SolInput {
  val claims = input("input03.txt")

  val parsedClaims = claims
    .map(s => """\d+""".r.findAllIn(s)
      .map(_.toInt).toList)
  val claimsArea = parsedClaims
    .map{case List(id,x,y,h,w) => id -> Range(x, x+h)
      .flatMap(x => Range(y, y+w).map(y => (x,y)))
    }

  val allClaimsArea = claimsArea.flatMap(_._2)

  val intersectedArea = allClaimsArea
    .groupBy(identity)
    .filter(_._2.length > 1)
    .keys.toSet

  val intersectedAreaCount = intersectedArea.count(_ => true)
  println(s"$intersectedAreaCount")

  val notIntercectedClaimID = claimsArea
    .filter{case (_, cells) =>
      cells.forall(coord => !intersectedArea.contains(coord))}
    .map(_._1).head
  println(s"$notIntercectedClaimID")
}
