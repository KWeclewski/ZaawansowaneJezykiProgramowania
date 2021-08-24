object Main extends App {
  //zad 1
  def subSeq[A](seq: Seq[A],begIdx: Int, endIdx: Int): Seq[A] = {
    if(begIdx < endIdx && begIdx > 0 && (endIdx <= seq.size - begIdx)){
      seq.drop(begIdx).take(endIdx-begIdx+1)
    }else{
      Seq[A]()
    }
  }

  //dane testowe
  var x = Seq[Int](0,10,10,10,10,10,0,0,1,3,4,5,5,5,5,5,5,6)
  var y = Seq[String]("ogorek","majtki","Wilk","Bozena","MordaTam")
  val z = Seq[Int](1,2,3,4)
  // print(subSeq(x,1,3))
  
  //zad 2 
  def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
    seq.take(k) ++ seq.zipWithIndex (wartosc, index)
          .filter{ x=> x._2 > k}
          .map(x => (x._1))
  }

  // print(remElems(y,3))
  //zad 3
  def isOrdered[A](seq: Seq[A])(leq: (A, A) => Boolean): Boolean = { 
    seq.sliding(2).map(s => leq(s(0), s(1))).foldLeft(true)(_ == _) 
  }
  // print(x.sliding(2).toList.foldLeft(0) ((z:Int,y:(Int,Int)) => (Seq[Int]()++z++(y._1<y._2)) ))
  // print(x.sliding(2).toList)
  // print(isOrdered(x)(_<_))

  //zad 4
  def freq[A](seq: Seq[A]) : Seq[(A, Int)] = {
    seq.groupBy(a => a).map(b => (b._1, b._2.length)).toSeq

  }
  // print(freq(x))

  //zad 5
  def deStutter[A](seq: Seq[A]): Seq[A] ={
    seq.foldLeft(Seq[A]()){(akumulator, element) => 
      if(akumulator.takeRight(1).equals(Seq[A](element))) akumulator else akumulator :+ element}
  }
  // print(deStutter(x))

  //zad6
  def diff[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
      seq1.zip(seq2).filter(x => x._1 != x._2).unzip._1
  }
  // val x1 = Seq[Int](1,2,3,2,2)
  // val x2 = Seq[Int](2,2,1,3)
  // print(diff(x2,x1))
}