import scala.collection.mutable

object Sol_22 extends App with SolInput {

  val inputData = input("input22.txt")
  val depth = inputData.head.replace("depth: ", "").toInt
  val Array(tx, ty) = inputData.last.replace("target: ", "").split(",").map(_.toInt)
  val target = Pos(tx,ty)

  //val depth = 510
  //val target = Pos(10,10)

  sealed trait Tools
  case object Torch extends Tools
  case object ClimbGear extends Tools
  case object Neither extends Tools

  sealed trait Region {
    def risk: Int
    def acceptedTools: Set[Tools]
  }
  case object Rocky extends Region {
    def risk: Int = 0
    def acceptedTools: Set[Tools] = Set(Torch, ClimbGear)
  }
  case object Wet extends Region {
    def risk: Int = 1
    def acceptedTools: Set[Tools] = Set(ClimbGear, Neither)
  }
  case object Narrow extends Region {
    def risk: Int = 2
    def acceptedTools: Set[Tools] = Set(Torch, Neither)
  }

  implicit class Regions(val reg: Region)
  {
    def toChar: Char = reg match {
      case Rocky => '.'
      case Wet => '='
      case Narrow => '|'
    }
  }

  type Grid[A] = Vector[Vector[A]]
  type ERGrid = Grid[Int]
  type RTGrid = Grid[Region]

  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  }

  object Pos {
    val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
  }

  val modulo = 20183
  val xTimes = 16807
  val yTimes = 48271
  val reserve = 8

  def ErosionLevel: ERGrid = {
    val el = mutable.Seq.fill(target.y + reserve + 1, target.x + reserve + 1)(-1)
    for (y <- 0 to target.y + reserve) {
      for (x <- 0 to target.x + reserve) {
        val geologicIndex = (y, x) match {
          case (0, 0) => 0
          case (y, x) if x == target.x && y == target.y => 0
          case (y, 0) => y * yTimes
          case (0, x) => x * xTimes
          case (y, x) => el(y)(x - 1) * el(y - 1)(x)
        }
        el(y)(x) = (geologicIndex + depth) % modulo
      }
    }
    el.toVector.map(_.toVector)
  }

  def Cave: RTGrid = {
    val el = ErosionLevel
    el.map(v => v.map(item => item % 3 match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }))
  }

  def RiskLevel: Int = Cave.zipWithIndex
      .filter(y => y._2 <= target.y)
      .map(v => v._1
        .zipWithIndex
        .filter(x => x._2 <= target.x)
        .map(item => item._1.risk).sum).sum

  def PrettyPrint = Cave.map(_.map(_.toChar).mkString).mkString("\n")

  println(PrettyPrint)

  println(RiskLevel)


}
