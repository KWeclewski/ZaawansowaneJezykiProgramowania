import scala.annotation.tailrec

object Main extends App {

  //Zad 1
  
  def isPrime(i: Int): Boolean = {
    
    @tailrec
    def isPrimeUntil(j: Int): Boolean =
      if (j <= 1) true
      else i % j != 0 && isPrimeUntil(j-1)
      isPrimeUntil(i / 2)
  }

  println(isPrime(14))
  println(isPrime(7))

  //zad 2
  def ciąg(n: Int): Int = {
    @tailrec
    def c(a: Int, b: Int,n: Int): Int = {
      if(n == 0){
        return a
      }
      c(b,a+b,n-1)
    }

    if( n == 0){
      0
    }
    else if (n == 1){
      1
    }
    else{
      c(0,1,n)
    }        
  }
  println(ciąg(8))
  
  //zad 3
  def uporządkowana(tab: Array[Int], mlr: (Int, Int) => Boolean): Boolean = {
  @tailrec
    def jestPorządekWTablicy(tab: Array[Int], jestPorządek: Boolean): Boolean = {
      if (!jestPorządek) return false
      else if (tab.size < 2) return true
      jestPorządekWTablicy(tab.dropRight(1), mlr(tab(tab.size - 2), tab(tab.size - 1)))
    }
    jestPorządekWTablicy(tab, true)
  }
  println(uporządkowana(Array(1, 2, 3), (xL: Int, xR: Int) => xL < xR))

  //zad 4
  def maksimum(ListPierwszy: List[Double], ListDrugi: List[Double]): List[Double] = {
    @tailrec
    def listaWewnetrzna(ListPierwszy: List[Double], ListDrugi: List[Double], ListWynikowa: List[Double]): List[Double]={
      if(List[Int](ListPierwszy.length,ListDrugi.length).min > 0){
        if(ListPierwszy.head >= ListDrugi.head){
          listaWewnetrzna(ListPierwszy.tail,ListDrugi.tail, (ListWynikowa ++ List[Double](ListPierwszy.head)))
        }else{
          listaWewnetrzna(ListPierwszy.tail,ListDrugi.tail, (ListWynikowa ++ List[Double](ListDrugi.head)))
        }
      }else{
        if(ListPierwszy.length > 0){
          return (ListWynikowa ++ ListPierwszy)
        }else{
          return (ListWynikowa ++ ListDrugi)
        }
      }
    }
    listaWewnetrzna(ListPierwszy,ListDrugi,List[Double]())
  }
  print(maksimum(List(2.0, -1.6, 3.2, 5.4, -8.4), List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5, -4.1, -0.4, 5.5)))
}