object Main extends App {

    val p1 = new Prog(1,3);
    val p2 = new Prog(2,3);
    val p3 = new Prog(2,2);
    val p4 = p1 * p2
    println(p4.inP(1))
    println(p4.inP(2))
    val p5 = p1 * p3
    println(p5.inP(4))
    val p6 = new Prog(1,3)
    val p7 = new Prog(2,4)
    val p8 = p6 * p7
    println(p8.inP(10))
    val p9 = p6 + p7
    println(p9.inP(10))
    val p10 = new Prog(10,10);
    println(p10.inP(40))
    println(p10.inP(3))
}








/*
object Main extends App {
  var x = new ArithmeticSequence(1,2)
  var m = new ArithmeticSequence(1,3)
  var z = new ArithmeticSequence(1,1)
  var p = new ArithmeticSequence(1,1)
  var list = List(1)
  var list1 = List(1)

  val a = p.progression(list,5)
  val b = m.progression(list1,2)
  println(a,b)
  println( m.unionAll(a,b))
  println( m.intersection(a,b))

 // z.in(z)
  //z.in(p)
  println()



}
*/
