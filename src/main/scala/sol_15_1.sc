case class Pos(x: Int, y: Int) extends Ordered[Pos]
{
  def compare(that: Pos) =
    if (y == that.y && x == that.x) 0
    else if (y == that.y) x - that.x
    else y - that.y
}

val pls = List(Pos(0,0),Pos(2,1),Pos(3,2),Pos(1,2),Pos(7,7),Pos(3,1),Pos(1,3))

pls.sorted