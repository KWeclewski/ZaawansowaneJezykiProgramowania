object Main extends App {
  //zad 1
  def sum(l: List[Option[Int]]): Option[Int] = {
    @annotation.tailrec
    def suma(s : Int, t: List[Option[Int]]) : Option[Int] = {
      if(t.size <= 0){
        Some(s)
      }else{
        t.head match {
          case Some(value) => suma(s+value, t.tail)
          case None => suma(s, t.tail)
        }
      }
    }
    suma(0, l);
  }

  val x : List[Option[Int]] = List(Some(12),None, None,Some(16))
  // println(sum(x))

  //zad 2
  def compose[A,B,C](f: A => B)(g: B => C): A => C = {
    a: A => g(f(a))
  }
  
  print(compose[Int, Int, Boolean](_*2)(_%2==0)(3))

  // def prod[A,B,C,D](f: A => C, g: B => D): (A, B) => (C, D) = {
  //   (A, B)  => (f(A) * g(B))
  // }

  // val StworzParePierwsza = (a: Int) => {
  //   (a*a).toDouble
  // }
  // val StworzPareDruga = (b: Float) => {
  //   b+10
  // }

  val StworzListeDlugosci = (i: Int) => { List.range(1,i+1) }
  val DlugoscTablicy = (j: List[Int]) => { j.length }
  // val test1 = compose(StworzListeDlugosci,DlugoscTablicy)
  // val test2 = prod(StworzParePierwsza,StworzPareDruga)
  // print(test2(2,3f))

  //zad 3
  //zad 4

  //zad 5
  def divide[A](list: List[A]): (List[A], List[A]) = {
    @annotation.tailrec
    def divideInside(listaWejsciowa: List[A], listaParzysta: List[A], listaNieParzysta: List[A], parzysta: Boolean): (List[A], List[A]) = {
      if(lisciotaWejswa.length > 0){
        if(parzysta.equals(false)){
          divideInside(listaWejsciowa.tail, listaParzysta, listaNieParzysta ++ List[A](listaWejsciowa.head), !parzysta)
        }else{
          divideInside(listaWejsciowa.tail, listaParzysta ++ List[A](listaWejsciowa.head), listaNieParzysta, !parzysta)
        }
      }else{
        return (listaParzysta, listaNieParzysta)
      }
    }
    divideInside(list, List[A](), List[A](), true)
  }

  // print(divide(List[Int](0,0,1,2,3,4,5,6,7,8)))
  // print(divide(List[Char]('a') ++ "lista".toCharArray()))

  //zad 6

  //Zdefiniuj następujące generyczne operacje na funkcjach
//"złożenie"
//def compose[A,B,C](f: A => B)(g: B => C): A => C
//"iloczyn"
//def prod[A,B,C,D](f: A => C, g: B => D): (A, B) => (C, D)
//"podniesienie operatora"
//def lift[A,T](op: (T,T) => T)(f: A => T, g: A => T): A => T

  //zad2
  //złożenie
  def compose[A, B, C](f: A => B)(g: B => C): A => C = { a: A =>
    g(f(a))
  }
  //iloczyn
  def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {
    (a: A, b: B) => (f(a), g(b))
  }
  //podniesienie operatora
  def lift[A, T](op: (T, T) => T)(f: A => T, g: A => T): A => T = {
    (a: A) => op(f(a), g(a))
  }
  println("zad2:")
  println("zlożenie")
  println(compose[Int,Int, Boolean](_+2: Int)(_ % 2 == 0)(2))
  println(compose[Int,Int, Boolean](_+1: Int)(_ % 2 == 0)(2))
  println("iloczyn")
  println(prod[Int, Int, Boolean, Boolean](_ == 2, _ == 2)(2, 3))
  println(prod[Int, Int, Boolean, Boolean](_ == 2, _ == 2)(6, 10))
  println("podniesienie operatora")
  println(lift[Int, Int](_ + _ + 1)(_ + 1, _ + 1)(1))
  println(lift[Int, Int](_ + _ + 1)(_ + 1, _ + 1)(2))
  println(lift[Int, Int](_ + _ + 1)(_ + 1, _ + 1)(3))
  println()

  //zad 3
  //Niech MSet[A] "wielozbiory" typu A, czyli "zbiory z krotnością występowania elementów"
  //type MSet[A] = A => Int
  //Zdefiniuj operacje: sumy, różnicy oraz części wspólnej dla wielozbiorów:
  //def plus[A](s1: MSet[A], s2: MSet[A]): MSet[A]
  //def minus[A](s1: MSet[A], s2: MSet[A]): MSet[A]
  //def częśćWspólna[A](s1: MSet[A], s2: MSet[A]): MSet[A]
  //Wykorzystaj funkcje zdefiniowane w rozwiązaniu Zadania 2!
  //definicja typu MSet
  type MSet[A] = A => Int
  def multi1[A](a: A): Int = a match {
    case 1 => 3
    case 2 => 4
    case 3 => 6
    case _ => 0
  }

  def multi2[A](a: A): Int = a match {
    case 1 => 4
    case 2 => 2
    case 3 => 6
    case _ => 0
  }
  def plus[S](s1: MSet[S], s2: MSet[S]): MSet[S] = {
      val f = (a: Int,b: Int) => a+b
      a: S => lift[S,Int](f)(s1,s2)(a)
    }
  val p = plus[Int](multi1,multi2)
  //println(p(1))

  def minus[S](s1: MSet[S], s2: MSet[S]): MSet[S] = {
      val f = (a: Int,b: Int) => a-b
      a: S => lift[S,Int](f)(s1,s2)(a)
    }
  val q = minus[Int](multi1,multi2)
  /*
  def częśćWspólna[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {

  }

 */
  println(p(2)) // 4 + 2 = 6
  println(q(2)) // 4 - 2 = 2
}
