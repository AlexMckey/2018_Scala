object Sol_02 extends App with SolInput {

  val codes = input("input02.txt")

  val twos = codes.count(_.groupBy(identity).map(_._2.length == 2).reduce(_||_))
  val threes = codes.count(_.groupBy(identity).map(_._2.length == 3).reduce(_||_))

  val res1 = twos * threes

  println(s"$res1")

  val res2 = codes
    .combinations(2)
    .map(arr => (arr.head.intersect(arr.last),
                 arr.head.zip(arr.last)
                   .count{case (s1, s2) => s1 == s2}))
    .maxBy(_._2)._1

  println(s"$res2")
}
