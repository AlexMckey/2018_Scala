trait SolInput {
  def input(filename: String): Stream[String] = {
    val input = this.getClass.getResource(filename).getFile
    val it = scala.io.Source.fromFile(input).getLines
    it.toStream
  }
}