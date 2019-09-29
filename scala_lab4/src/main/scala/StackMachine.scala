abstract class Operation[T] {
  def op1(x: T, y: T): T
  def op2(x: T, y: T): T
  def op3(x: T, y: T): T
  def op4(x: T, y: T): T
}
abstract class Operation2[T] {
  def op1(x: T, y: T): T
  def op2(x: T, y: T): T
  def op3(x: T, y: T): T
  def op4(x: T, y: T): T
}
object Operation2 {
  implicit object BooleanStack extends Operation2[Boolean]{
    override def op1(x: Boolean, y: Boolean): Boolean = x || y

    override def op2(x: Boolean, y: Boolean): Boolean = x != y

    override def op3(x: Boolean, y: Boolean): Boolean = x && y

    override def op4(x: Boolean, y: Boolean): Boolean = x == y

  }

}
object Operation{
  implicit object IntStack extends Operation[Int]{
    override def op1(x: Int, y: Int): Int = x / y

    override def op2(x: Int, y: Int): Int = x + y

    override def op3(x: Int, y: Int): Int = x * y

    override def op4(x: Int, y: Int): Int = x - y
  }
  implicit object LongStack extends Operation[Long]{
    override def op1(x: Long, y: Long): Long = x / y

    override def op2(x: Long, y: Long): Long = x + y

    override def op3(x: Long, y: Long): Long= x * y

    override def op4(x: Long, y: Long): Long = x - y
  }
  implicit object DoubleStack extends Operation[Double]{
    override def op1(x: Double, y: Double): Double = x / y

    override def op2(x: Double, y: Double): Double= x + y

    override def op3(x: Double, y: Double): Double = x * y

    override def op4(x: Double, y: Double): Double= x - y
  }
  implicit object FloatStack extends Operation[Float]{
    override def op1(x: Float, y: Float): Float = x / y

    override def op2(x: Float, y: Float): Float= x + y

    override def op3(x: Float, y: Float): Float = x * y

    override def op4(x: Float, y: Float): Float= x - y
  }

}

class StackMachine[T] private(private val arr: List[T]) {
  def this() = this(List[T]())
  def print() = println(arr)
  def push(e: T): StackMachine[T] = new StackMachine(e::arr)
  def pushList(l: List[T]): StackMachine[T] = new StackMachine(l.reverse ::: arr)
  def pop(): (StackMachine[T], Option[T]) = arr match {
    case Nil => (this, None)
    case x::xs => (new StackMachine(xs), Option[T](x))
  }
  def div(implicit func: Operation[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op1(a, b)::xs)
    case _ => this
  }
  def add(implicit func: Operation[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op2(a, b)::xs)
    case _ => this
  }
  def mul(implicit func: Operation[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op3(a, b)::xs)
    case _ => this
  }
  def diff(implicit func: Operation[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op4(a, b)::xs)
    case _ => this
  }
  def union(implicit func: Operation2[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op1(a, b)::xs)
    case _ => this
  }
  def notEqu(implicit func: Operation2[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op2(a, b)::xs)
    case _ => this
  }
  def and(implicit func: Operation2[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op3(a, b)::xs)
    case _ => this
  }
  def equ(implicit func: Operation2[T]): StackMachine[T] = arr match {
    case a::b::xs => new StackMachine[T](func.op4(a, b)::xs)
    case _ => this
  }


}
object main extends App {
  var a = new StackMachine[Double]()
  a = a.push(2)
  a = a.push(3)
  a = a.mul
  println(a.pop()._2)
  a = a.push(3)
  a = a.push(5)
  a.print()
  var c = a.pop()
  println(c._2)
 var b = new StackMachine[Boolean]()
  b = b.pushList(List(true, false,true))
  b = b.push(false)
  b = b.union
  b = b.union
  b.print()
}