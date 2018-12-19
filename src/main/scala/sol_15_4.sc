case class Units(unitType: Char, pos: Pos, hp: Int = 200, ap: Int = 3)

case class Pos(x: Int, y: Int) {
  def distance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  def compare(second: Pos): Int =
    if (y == second.y && x == second.x) 0
    else if (y == second.y) x - second.x
    else y - second.y
}

object Pos {
  val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
  val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
  val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
  def compare(first: Pos, second: Pos): Int =
    if (first.y == second.y && first.x == second.x) 0
    else if (first.y == second.y) first.x - second.x
    else first.y - second.y
}
implicit val posReadingOrdering: Ordering[Pos] = Pos.compare(_,_)

def Parse(input: Iterator[String]): scala.collection.mutable.Map[Pos,Char] = {
  val map = scala.collection.mutable.Map.empty[Pos, Char]
  input.zipWithIndex.foreach {
    case (row, y) => row.zipWithIndex.foreach {
      case (cell, x) => map += Pos(x, y) -> cell
        map.withDefaultValue('#')
    }
  }
  map.withDefaultValue('#')
}

val input = "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######"
val board = Parse(input.lines)
def elves: List[Units] = board.collect{ case (pos,'E') => Units('E', pos)}.toList
def goblins: List[Units] = board.collect{ case (pos,'G') => Units('G', pos)}.toList
def mobs: List[Units] = (elves ++ goblins).sortBy(_.pos)

implicit val grid: scala.collection.mutable.Map[Pos,Char] = board
implicit val units: List[Units] = mobs

val e = Pos(1,1) // E
val g = Pos(2,3) // G
val g1 = Pos(5,3) // G1

grid += g1 -> '%'
grid.update(g1,'@')

def getTargets(unit: Units)(implicit units: List[Units]): Set[Units] =
  units.filter(u => u.unitType != unit.unitType).toSet

def isFree(pos: Pos)(implicit grid: scala.collection.mutable.Map[Pos,Char], units: List[Units]): Boolean = {
  //grid(pos) == '.' && !units.exists(_.pos == pos)
  grid(pos) == '.'// && !units.exists(_.pos == pos)
}

def getInRange(targets: Set[Units])(implicit grid: scala.collection.mutable.Map[Pos,Char], units: List[Units]): Set[Pos] = {
  for {
    target <- targets
    offset <- Pos.axisOffsets
    pos = target.pos + offset
    if isFree(pos)
  } yield pos
}

val tgs = getTargets(units.find(u => u.pos == e).get)
getTargets(units.find(u => u.pos == g).get)

isFree(e)
isFree(g)
Pos.axisOffsets.map(_ + e).map(pos => pos -> isFree(pos))
Pos.axisOffsets.map(_ + g).map(pos => pos -> isFree(pos))
Pos.axisOffsets.map(_ + g1).map(pos => pos -> isFree(pos))
grid(g1)

def bfs(startPos: Pos, endPos: Set[Pos])(implicit grid: scala.collection.mutable.Map[Pos,Char], units: List[Units]): Map[Pos, Int] = {
  def bfsLoop(visited: Map[Pos, Int], toVisit: Map[Pos, Int]): Map[Pos, Int] = {
    val neighbors = for {
      (pos, dist) <- toVisit
      offset <- Pos.axisOffsets
      newPos = pos + offset
      if isFree(newPos)
    } yield newPos -> (dist + 1)
    val newVisited = visited ++ toVisit
    val newToVisit = neighbors -- visited.keys
    if (newToVisit.isEmpty || (toVisit.keySet intersect endPos).nonEmpty)
      newVisited
    else
      bfsLoop(newVisited, newToVisit)
  }

  bfsLoop(Map.empty, Map(startPos -> 0))
}
var ir = tgs.map(u => u.pos)

bfs(e,ir)

val rch = bfs(e, ir).filterKeys(ir)

val minDist = rch.values.min
rch.filter(_._2 == minDist).keySet