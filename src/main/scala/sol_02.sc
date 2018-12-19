import scala.io.Source.fromFile

val test = "xrecqmdonskvzupalfkwhjctdb"

val ss = "abcdef;bababc;abbcde;abcccd;aabcdd;abcdee;ababab"
val sl = ss.split(';')

val s = sl(1)

val twos = sl.count(_.groupBy(identity).map(_._2.length == 2).reduce(_||_))
val threes = sl.count(_.groupBy(identity).map(_._2.length == 3).reduce(_||_))

val codes = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz".split('\n')
//val codess = fromFile("D:\\DevsExercises\\AdventOfCode\\AdventOfCode_2018-master\\2018_Scala\\src\\main\\scala\\input02.txt")
//  .getLines().toSeq

//val cc = codess
//  .combinations(2)
//  //.take(2)
//  .map(arr => (arr.head, arr.last, arr.head.zip(arr(1)).count(p => p._1 == p._2))).maxBy(_._3)
//val (w1, w2, _) = cc
//val res2 = w1.intersect(w2)
//res2 == "xretqmmonskvzupalfiwhcfdb"

val v = "bababc"
v.groupBy(c => c).map{case (o, str) => (o, str.length)}