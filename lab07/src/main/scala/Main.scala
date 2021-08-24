import scala.io.Source

object Main extends App {
  //zad 1
  val nazwa: List[String] = Source.fromFile("liczby.txt").getLines.toList
  val wtf = nazwa.map(_.toList).filter{x: List[Char] => 
    println(x.sliding(2).toList)
    x.sliding(2)
    .map{s  => s(0).asDigit <= s(1).asDigit}
    .foldLeft(true)(_ == _)}
    println("WTF") //not working yet
    wtf.map(a => println(a.sliding(2).toList))
  println(wtf)

  //zad 2
  import scala.io.Source
  val ludzie: List[String] = Source.fromFile("osoby.txt").getLines.toList
   
  def search(people: List[String]): List[String] = {
    val max_names = people.map(s => s.split(" ", 2).toList).sortBy(s => (s(0).toLowerCase.foldLeft(Set[Char]()){
      case (set, c) if (set.contains(c) == true) => set
      case (set, c) if (set.contains(c) == false) => set++Set(c)
      }.size -> s(1).length)).maxBy(_(0).length)
    List(max_names).minBy(_(1).length)
  }

  println(search(ludzie))

  //zad 3
  import scala.io.Source

  val ksiazka: List[String] = Source.fromFile("ogniem_i_mieczem.txt").getLines.toList

  def licz(chapter: List[String]): Seq[(Char, Int)] = {
    chapter.map(s => s.toLowerCase.toSeq.filter(c => c.isLetter)).flatten.groupBy(identity).mapValues(_.size).filter(m => m != Map()).toSeq.sorted
  }

  def histogram(max: Int)(policzone: Seq[(Char, Int)]): Unit = {
    policzone.foreach(c => {
      val s: String = if (c._2 <= max) List.fill(c._2)('*').mkString else List.fill(max)(' ').mkString
      println(c._1.toString + ':' + s)
    })
  }
  histogram(100)(licz(ksiazka))

}

// import scala.io.Source

//     val liczby: List[String] = Source.fromFile("liczby.txt").getLines.toList

//     def count(numbers: List[String]): Int = {
//         numbers.map(n => ((n.toList.foldLeft(0)(_ + _) % 2) == 1, n.sliding(2).map(s => s(0) < s(1)).foldLeft(true){
//             case (p, e) if (p == false) => false
//             case (p, true) if (p == true) => true
//             case (p, false) if (p == true) => false
//         })).filter(s => s == (true, true)).length
//     }
    
//     println(count(liczby))
// }