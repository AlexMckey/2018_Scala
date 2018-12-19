trait BoardObjects
case object Wall extends BoardObjects
case object Empty extends BoardObjects
trait Unit extends BoardObjects {def HP: Int}
case class Elves(HP: Int) extends BoardObjects
case class Goblin(HP: Int) extends BoardObjects
def toBoardObjects(ch: Char): BoardObjects = ch match {
  case '#' => Wall
  case '.' => Empty
  case 'G' => Goblin(200)
  case 'E' => Elves(200)
}
def toChar(obj: BoardObjects): Char = obj match {
  case Wall => '#'
  case Empty => '.'
  case Elves(_) => 'E'
  case Goblin(_) => 'G'
  case _ => ' '
}
case class Pos(x: Int, y: Int) extends Ordered[Pos]
{
  def compare(that: Pos) =
    if (y == that.y && x == that.x) 0
    else if (y == that.y) x - that.x
    else y - that.y
}
def toBoard(input: Stream[String]): Map[Pos, BoardObjects] =
  input.zipWithIndex
    .flatMap{ case (str, y) => str.zipWithIndex
      .map{ case (ch, x) => Pos(x,y) -> toBoardObjects(ch)}}
    .toMap.withDefaultValue(Wall)
def PrettyPrintBoard(b: Map[Pos, BoardObjects]) = {
  b.toList
    .map{case(pos,obj) => (pos -> toChar(obj))}
    .sortBy{case(pos,ch) => pos}
    .grouped(b.count{case(pos,_) => pos.y == 0})
    .map(lst => lst.map{case(_,ch) => ch}.mkString)
    .foreach(println)
}

val input = "#######;#E..G.#;#...#.#;#.G.#G#;#######".split(';').toStream
val board = toBoard(input)

board(Pos(-1,-1))
PrettyPrintBoard(board)
