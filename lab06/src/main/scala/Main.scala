import collection.immutable.SortedSet
object Main extends App {
  //zad 1
  def indices[A](seq: Seq[A], el: A): Set[Int] = {
    seq.zipWithIndex.filter{ x=> x._1 == el}.unzip._2.toSet
  }

  val x = Seq[Int](-2,-4,0,1,2,5,6)
  val y = Set[Int](-2,-4,0,1,2,5,6)
  // print(indices(x,3))

  //zad 2
  def minNotContained(set: Set[Int]): Int = {
    set.filter(i => i >= 0)
      .toList
      .sortWith(_<_)
      .zipWithIndex
      .filter(j => j._1 != j._2)
      .take(1).head._2
  }
  // print(minNotContained(y))

  //zad 3
  def swap[A](seq: Seq[A]): Seq[A] = {
    seq.grouped(2)
      .flatMap{
        case Seq(x,y) => Seq(y,x)
        case Seq(x) => Seq(x)
      }.toSeq
  }
  // print(swap(x))

  //zad 4
  def strefyCzasowe(strefy: Seq[String]): Seq[String] = {
    strefy.filter(x => x.startsWith("Europe"))
      .map(y => y.stripPrefix("Europe/"))
      .groupBy(_.length())
      .toSeq
      .sortWith(_._1 < _._1)
      .map(x => x._2.sortWith(_ < _))
      .toSeq
      .reduce(_ ++ _)
  }
  val strefyczasowe: Seq[String] = java.util.TimeZone.getAvailableIDs.toSeq
  // print(strefyCzasowe(strefyczasowe))

  //zad 5
  def score(code: Seq[Int])(move: Seq[Int]): (Int,Int) = {
    (code.zip(move).filter(x => x._1 == x._2).length,
    code.intersect(move).groupBy(a => a).toSeq.length)
  }
  val code = Seq(1,3,2,2,4,5)
  val move = Seq(2,1,2,4,7,2)
  // print(score(code)(move))
}
