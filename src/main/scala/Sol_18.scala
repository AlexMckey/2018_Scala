object Sol_18 extends App{

  type Grid[A] = Vector[Vector[A]]

  case class Pos(x: Int, y: Int) {
    def manhattanDistance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  }

  object Pos {
    val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
    val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
    val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
  }

  implicit class HeadIterator[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next) else None
    def head: A = headOption.get
  }

  implicit class GridOps[A](grid: Grid[A]) {
    def rotateCW: Grid[A] = {
      val h = grid.size
      val w = grid(0).size

      /*
      1 2 3
      4 5 6
      4 1
      5 2
      6 3
       */

      Vector.tabulate(w, h)((y, x) => grid(h - 1 - x)(y))
    }

    def flipV: Grid[A] = grid.reverse

    def rotations: Set[Grid[A]] = (1 to 3).scanLeft(grid)({ case (acc, _) => acc.rotateCW }).toSet

    def symmetries: Set[Grid[A]] = grid.rotations ++ grid.flipV.rotations

    def groupedGrid(groupSize: Int): Grid[Grid[A]] =
      grid.grouped(groupSize).map(_.map(_.grouped(groupSize).toVector).transpose).toVector

    def mapGrid[B](f: A => B): Grid[B] = grid.map(_.map(f))

    def flattenGrid[B](implicit asGrid: A => Grid[B]): Grid[B] =
      grid.mapGrid(asGrid).flatMap(_.transpose.map(_.flatten))

    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum
  }

  type Memory = IndexedSeq[Int]

  trait Solution {
    def reallocCycleCount(initialMemory: Memory): Int
    def reallocCycleLoop(initialMemory: Memory): Int
  }

  object FloydSolution extends Solution {
    /**
      * https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
      */
    def floyd[A](x0: A, f: A => A): (Int, Int) = {
      var tortoise = f(x0)
      var hare = f(f(x0))
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(f(hare))
      }

      var μ = 0
      tortoise = x0
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(hare)
        μ += 1
      }

      var λ = 1
      hare = f(tortoise)
      while (tortoise != hare) {
        hare = f(hare)
        λ += 1
      }

      (μ, λ)
    }

    override def reallocCycleCount(initialMemory: Memory): Int = {
      val (μ, λ) = floyd(initialMemory, reallocCycle)
      μ + λ
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      val (μ, λ) = floyd(initialMemory, reallocCycle)
      λ
    }
  }

  def reallocCycle(memory: Memory): Memory = {
    val (max, maxIndex) = memory.view.zipWithIndex.maxBy(_._1)
    val d = max / memory.size
    val r = max % memory.size
    val wrapAround: Boolean = maxIndex + r >= memory.size

    memory.updated(maxIndex, 0).map(_ + d).zipWithIndex.map({ case (x, i) =>
      if (maxIndex < i && i <= (maxIndex + r))
        x + 1
      else if (wrapAround && i <= (maxIndex + r) % memory.size)
        x + 1
      else
        x
    })
  }

  implicit class GridOps2[A](grid: Grid[A]) {
    def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = {
      grid.sliding(size).map({ rows =>
        rows.map(_.sliding(size).toVector).transpose.toIterator
      })
    }
  }

  implicit class PosGrid[A](grid: Vector[Vector[A]]) {
    def apply(pos: Pos): A = grid(pos.y)(pos.x)
  }

  implicit class PosGrid2[A](grid: Grid[A]) {
    def containsPos(pos: Pos): Boolean = grid.indices.contains(pos.y) && grid(pos.y).indices.contains(pos.x)
  }

  def step(grid: Grid[Char]): Grid[Char] = {
    /*val paddingRow = Vector.fill(grid(0).size + 2)('.')
    val paddedGrid: Grid[Char] = paddingRow +: grid.map('.' +: _ :+ '.') :+ paddingRow
    def stepTile(grid: Grid[Char]): Char = {
      val neighbors = (grid(0) ++ Vector(grid(1)(0), grid(1)(2)) ++ grid(2)).groupBy(c => c).mapValues(_.length).withDefaultValue(0)
      grid(1)(1) match {
        case '.' if neighbors('|') >= 3 => '|'
        case '|' if neighbors('#') >= 3 => '#'
        case '#' if neighbors('|') >= 1 && neighbors('#') >= 1 => '#'
        case '#' => '.'
        case c => c
      }
    }
    paddedGrid.slidingGrid(3).map(_.map(stepTile).toVector).toVector*/

    (for ((row, y) <- grid.zipWithIndex.par)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Pos(x, y)
          val neighbors = Pos.allOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
          val trees = neighbors.count(_ == '|')
          val lumberyards = neighbors.count(_ == '#')
          cell match {
            case '.' if trees >= 3 => '|'
            case '|' if lumberyards >= 3 => '#'
            case '#' if trees >= 1 && lumberyards >= 1 => '#'
            case '#' => '.'
            case c => c
          }
        }).seq
  }

  def resourceValue(grid: Grid[Char]): Int = {
    val trees = grid.countGrid(_ == '|')
    val lumberyards = grid.countGrid(_ == '#')
    trees * lumberyards
  }

  def resourceValueIterate(grid: Grid[Char], after: Int = 10): Int = {
    val it = Iterator.iterate(grid)(step)
    val finalGrid = it.drop(after).head
    resourceValue(finalGrid)
  }

  def resourceValueCycle(grid: Grid[Char], after: Int = 1000000000): Int = {
    val (mu, lambda) = FloydSolution.floyd(grid, step)
    val afterMu = (after - mu) % lambda
    resourceValueIterate(grid, mu + afterMu)
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }


  def parseInput(input: String): Grid[Char] = input.lines.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("input18.txt")).mkString.trim


  println(resourceValueIterate(parseInput(input)))
  println(resourceValueCycle(parseInput(input)))

}
