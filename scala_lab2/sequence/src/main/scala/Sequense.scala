
//для бесконечных ариф прогрессий

    class Prog private(pf: Int => Boolean){
        val f = pf

        def this(a:Int,b:Int) = this(x => (x - a) >= 0 && (x - a) % b ==0)

//объединение арифметических прогрессий

        def +(other: Prog): Prog ={
            new Prog(x => f(x) || other.f(x))
        }

//пересечение арифметических прогрессий

        def *(other: Prog): Prog ={
            new Prog(x => f(x) && other.f(x))
        }
//проверка на пренадлежание

        def inP(x: Int): Boolean = f(x)
     }










//для конечных прогрессий
/*

class ArithmeticSequence (firstNumber: Int, diff: Int){
    val x = firstNumber
    val d = diff
    val list = List()
    val p = Int

//объединение внутри одной последовательности

    val progression: (List[Int],Int) => List[Int] = {
           case (list, p) if (p > 0)
           => (progression((x + (p-1) * diff :: list).sorted , p - 1):::list).distinct
           case (list, p)  if (p <= 0) => Nil
    }


//объединение арифметических прогрессий

    def unionAll(set1: List[Int], set2: List[Int]) = {
             println(set1, set2)
             val b = (set1 ::: set2).distinct
             b
        }

//пересечение ариф после

    def intersection(set1: List[Int], set2: List[Int])= {
        val s = set1.intersect(set2)
        s
    }

//существование в арифметической прогрессии

    def in(m: ArithmeticSequence, set1: List[Int]):Unit = {
      set1.filter(_ == m.x)
    }
}
*/