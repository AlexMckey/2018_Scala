import scala.collection.mutable
import scala.collection.mutable.Map

sealed abstract class BoardObjects

case object Wall extends BoardObjects

case object Empty extends BoardObjects

case class Obj(ch: Char) extends BoardObjects

trait Units_ extends BoardObjects {def HP: Int; var hasMoves: Boolean}

case class Elves(HP: Int, var hasMoves: Boolean) extends Units_

case class Goblin(HP: Int, var hasMoves: Boolean) extends Units_

object BoardObjects {
  implicit class PrettyPrint(val obj: BoardObjects){
    def toChar: Char = obj match {
      case Wall => '#'
      case Empty => '.'
      case Elves(_,_) => 'E'
      case Goblin(_,_) => 'G'
      case Obj(ch) => ch
      case _ => ' '
    }
  }

}

case class Pos(x: Int, y: Int) extends Ordered[Pos]
{
  def +(that: Pos) = Pos(x + that.x, y + that.y)
  def +(p: (Int, Int)) = Pos(x + p._1, y + p._2)
  def near(): List[Pos] = List(this + (0, -1), this + (-1,0), this + (1, 0), this + (0,1))
  def distance(that: Pos): Int = Math.abs(y - that.y) + Math.abs(x - that.x)
  def compare(that: Pos): Int =
    if (y == that.y && x == that.x) 0
    else if (y == that.y) x - that.x
    else y - that.y
  def nearPos(pos: Pos): List[Pos] = pos.near
}

object Pos {
  val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
  val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
  val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
}

trait Board {
  def Parse(input: Stream[String]): Map[Pos, BoardObjects] = Map().withDefaultValue(Wall) ++
    input.zipWithIndex
      .flatMap { case (str, y) => str.zipWithIndex
        .map { case (ch, x) => Pos(x, y) -> ch.toBoardObjects }
      }
      .toMap
  implicit class Parser(val ch: Char) {
    def toBoardObjects: BoardObjects = ch match {
      case '#' => Wall
      case '.' => Empty
      case 'G' => Goblin(200, true)
      case 'E' => Elves(200, true)
      case ch => Obj(ch)
    }
  }
}
class NewGameBoard(input: Stream[String]) extends Board {
  def elves: List[(Pos, Elves)] = board.collect{ case (pos, e:Elves) => (pos, e)}.toList.sortBy(_._1)
  def goblins: List[(Pos, Goblin)] = board.collect{ case (pos, g:Goblin) => (pos, g)}.toList.sortBy(_._1)
  def units: List[(Pos, Units_)] = (elves ++ goblins).sortBy(_._1)
  def nearEmptyPos(pos: Pos): List[Pos] = pos.near.filter(p => board(p) == Empty)
  def nearObj(pos: Pos): List[BoardObjects] = pos.near.map(p => board(p))
  def PrettyPrintBoard(): Unit =
    board.toList.map{case(pos,obj) => (pos -> obj.toChar)}
      .sortBy{case(pos,_) => pos}
      .grouped(board.count{case(pos,_) => pos.y == 0})
      .map(lst => lst.map{case(_,ch) => ch}.mkString)
      .foreach(println)
  val board: mutable.Map[Pos, BoardObjects] = Parse(input)

  def findNearestEnemy(unitPos: Pos): Option[Pos] = {
    val enemies = (board(unitPos) match {
      case _: Goblin => elves
      case _: Elves => goblins
    }).map(_._1)
    val nearsPos = enemies.flatMap(pos => nearEmptyPos(pos))
    if (nearsPos.isEmpty) None
    else Some(nearsPos
      .map(pos => (pos -> pos.distance(unitPos)))
      .minBy(_._2)._1)
  }

  def findToMove(myPos: Pos, enemyPos: Pos): Option[Pos] = {
    val nearsPos = nearEmptyPos(myPos)
    if (nearsPos.isEmpty) None
    else Some(nearsPos.map(pos => (pos -> pos.distance(enemyPos)))
      .minBy(_._2)._1)
  }

  def move(unit: (Pos, Units_)): Option[Pos] =
  {
    val e = findNearestEnemy(unit._1)
    val res = if (e.isEmpty) None
    else {
      val m = findToMove(unit._1, e.get)
      if (m.isEmpty) None
      else {
        if (e.get == unit._1) None
        else m
      }
    }
    res
  }

  def oneStep(): Unit =
  {
    val unitsToMove = units.filter{case(_, o) => o.hasMoves}
    unitsToMove.foreach { case u@(pos, o) =>
      val m = move(u)
      if (m.isDefined)
      {
        board += pos -> Empty
        board += m.get -> o
      }
      else
      {
        //board(pos).asInstanceOf[Units].hasMoves = false
        o.hasMoves = false
      }
    }
  }
}

object Sol_15 extends App with SolInput {
  implicit class PrettyNums(val num: Int){
    def toNumChar: Char = num.toString()(0)
  }
  //val gb = new GameBoard(input("input15.txt"))
  val input = "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########".split('\n').toStream
  var gb = new NewGameBoard(input)
  gb.PrettyPrintBoard()
//  println(gb.elves)
//  println(gb.goblins)
//  val elv = gb.elves.head
//  val gp = gb.goblins.flatMap{case(pos,_) => gb.nearReachablePos(pos)}
//  val gpd = gp.map(pos => pos -> Obj(pos.distance(elv._1).toNumChar))
//  val gfp = gpd.sortBy(_._1).map(_._1).head
//  gb.board += (gfp -> Obj('*'))
//  val efp = gb.nearReachablePos(elv._1).map(pos => (pos -> pos.distance(gfp))).sortBy(_._1).head._1
//  gb.board += (efp -> Obj('+'))
//  gb.PrettyPrintBoard()

  gb.oneStep()
  gb.PrettyPrintBoard()
  gb.oneStep()
  gb.PrettyPrintBoard()
  gb.oneStep()
  gb.PrettyPrintBoard()
  gb.oneStep()
  gb.PrettyPrintBoard()
}