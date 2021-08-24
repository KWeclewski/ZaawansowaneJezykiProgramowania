import scala.annotation.tailrec

object Main extends App {

  //Zad 1
  def długość(str: String): Int = {
    if(str == ""){
      0;
    }else{
      długość(str.tail) + 1;
    }
  }
  // println(długość("każdy"))

  //Zad 2
  def jestPalindromem(tab: Array[Int]): Boolean = {
    if(tab.length == 1 || tab.length == 0){
      true
    }else{
      if(tab(0) == tab(tab.length-1)){
        jestPalindromem((tab.slice(1, tab.length-1)))
      }else{
        false
      }
    }
  }

  // println(jestPalindromem(Array(1,2,1)))

  //zad 3
  def trójkąt(wys: Int): Unit = {
    if (wys > 0) {
      trójkąt(wys - 1)
    }
    for (j <- 0 to wys) {
      print(" " + symbolNewton(wys, j))
    }
    println();
  }

  def silnia(n: Int): Int = n match {
    case 0 => 1
    case _ => n * silnia(n - 1)
  }
  
  def symbolNewton(n: Int, r: Int): Int = {
    silnia(n) / (silnia(n - r) * silnia(r))
  }
  trójkąt(0)
  trójkąt(6)
  trójkąt(9)

  //Zad 4
  def jestPierwsza(n: Int): Boolean = {
    @annotation.tailrec
    def prime(s: Int, e: Int): Boolean = {
      if (s > e) {
        true
      } else if (n % s != 0) {
        prime(s + 1, e)
      } else {
        false
      }
    }

    if (n < 2) {
      false
    } else {
      prime(2, math.sqrt(n).toInt)
    }

  }
  def daSię(n: Int): Boolean = {
    val array = new Array[Boolean](n + 1)
    for (i <- 3 to n) {
      array(i) = jestPierwsza(i)
    }
    if (n <= 4) {
      return false
    }
    for (x <- 3 to n) {
      if (x % 2 == 0) {
        for (y <- 3 until array.size) {
          if (array(y) == true) {
            for (z <- j until array.size) {
              if (array(z) == true) {
                if (y + k == i) {
                  println(y + " + " + k + " = " + i + " ")
                }
              }
            }
          }
        }
      }
    }
    return true
  }
  print(daSię(3))
}
