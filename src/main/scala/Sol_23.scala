object Sol_23 extends App with SolInput {

  case class Pos3D(x: Int, y: Int, z: Int) {
    def Distance(that: Pos3D): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    def +(that: Pos3D): Pos3D = Pos3D(x + that.x, y + that.y, z + that.z)
    def *(k: Int): Pos3D = Pos3D(x * k, y * k, z * k)
  }

  object Pos3D {
    val near: Seq[Pos3D] = Seq(Pos3D(0, 0, 1),
                               Pos3D(0, 0, -1),
                               Pos3D(0, 1, 0),
                               Pos3D(0, -1, 0),
                               Pos3D(1, 0, 0),
                               Pos3D(-1, 0, 0))}

  case class Bot(pos: Pos3D, r: Int)

  val file = this.getClass.getResource("input23.txt").getFile
  val input = scala.io.Source.fromFile(file).getLines()
  val inputData = input
      .map(_.replace("pos=<","")
        .replace(">, r=",","))
      .map(_.split(",")
        .map(_.toInt))


  implicit val nbs: Seq[Bot] = inputData.map{case Array(x,y,z,r) => Bot(Pos3D(x,y,z),r)}.toSeq

  val botWithLargestRadius = nbs.maxBy(nb => nb.r)
  val res1 = nbs.count(nb => nb.pos.Distance(botWithLargestRadius.pos) <= botWithLargestRadius.r)
  println(res1)

  val startPos = nbs.reduce((nb1,nb2) => Bot(Pos3D((nb1.pos.x + nb2.pos.x) / 2,
                                                   (nb1.pos.y + nb2.pos.y) / 2,
                                                   (nb1.pos.z + nb2.pos.z) / 2), 0)).pos

  implicit val center: Pos3D = Pos3D(0,0,0)

  def DistanceTo0(pos: Pos3D)(implicit center: Pos3D): Int = center.Distance(pos)

  def NumberBotsSeeThatPos(pos: Pos3D)(implicit bots: Seq[Bot]): Int =
    bots.count(nb => nb.pos.Distance(pos) <= nb.r)

  def IsBetter(curPos: Pos3D, candidatePos: Pos3D): Boolean =
  {
    val cntBot = NumberBotsSeeThatPos(curPos)
    val cntCandidate = NumberBotsSeeThatPos(candidatePos)
    cntCandidate > cntBot || (cntBot == cntCandidate
      && DistanceTo0(candidatePos) < DistanceTo0(curPos))
  }

  def BinaryStepIter(startPos: Pos3D, neighbour: Pos3D): Iterator[(Boolean, Int, Pos3D)] =
    Iterator.iterate((false, 2, startPos)){
      case (_, step, pos) =>
        val candidate = pos + neighbour * step
        if (IsBetter(pos, candidate))
          (false, step * 2, candidate)
        else
          (true, step, pos)
    }
  def FindBetterDirection(startPos: Pos3D): Seq[Pos3D] =
    Pos3D.near.filter(n => IsBetter(startPos, startPos + n))

  def FindPosWithStrongestBotOverlap(start: Pos3D)(implicit bots: Seq[Bot], center: Pos3D): Pos3D = {
    var curPos: Pos3D = start.copy()
    var done = false
    while (!done){
      val directions = FindBetterDirection(curPos)
      done = directions.isEmpty
      if (!done)
        for (n <- directions) {
          val it = BinaryStepIter(curPos + n, n)
          curPos = it
            .dropWhile{case(stop,_,_) => !stop}
            .map{case(_,_,pos) => pos}.next
        }
    }
    curPos
  }

  val res2 = DistanceTo0(FindPosWithStrongestBotOverlap(startPos))
  println(res2)
}