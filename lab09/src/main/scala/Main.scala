object Main extends App {
  //zad 1
  class Point( x: Double, y: Double) {
    def getX : Double = {
      this.x
    }
    def getY : Double = {
      this.y
    }

    private def odlegloscOdZera(tmp : Point) : Double = {
      tmp.getX * tmp.getX + tmp.getY * tmp.getY
    }

    override def equals(tmp: Any): Boolean = {
      tmp match{
        case tmp: Point =>{
          odlegloscOdZera(this) == odlegloscOdZera(tmp)
        }
        case _ => false
      }
    }
    
    def !=(tmp: Point): Boolean ={
      odlegloscOdZera(this) != odlegloscOdZera(tmp)
    }
    
    def >(tmp: Point): Boolean ={
      odlegloscOdZera(this) > odlegloscOdZera(tmp)
    }
    
    def <(tmp: Point): Boolean ={
      odlegloscOdZera(this) < odlegloscOdZera(tmp)
    }
    
    def <=(tmp: Point): Boolean ={
      odlegloscOdZera(this) <= odlegloscOdZera(tmp)
    }
    
    def >=(tmp: Point): Boolean ={
      odlegloscOdZera(this) >= odlegloscOdZera(tmp)
    }
  }

  var p1 = new Point(1,-1)
  var p2 = new Point(-1,1)

  // println(p1 <= p2)

  //zad 2,3,4
  abstract class MyList[+A] {
    def head: A
    def tail: MyList[A]
    def isEmpty: Boolean
    def add[B >: A](temp: B): MyList[B]
  }

  //rozszerzenie MyList w MyEmptyList
  protected class MyEmptyList extends MyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: MyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[A >: Nothing](temp: A): MyList[A] = new MyNonEmptyList[A](temp, this)
    override def toString = "[]"
  }

  //rozszerzenie MyList w MyNonEmptyList
  protected class MyNonEmptyList[A](hd: A, tl: MyList[A]) extends MyList[A] {
    def head: A = hd
    def tail: MyList[A] = tl
    def isEmpty: Boolean = false
    def add[B >: A](temp: B): MyList[B] = new MyNonEmptyList[B](temp, this)
    override def toString = {
      def printAllElements(str: String, lst: MyList[A]): String = lst match {
          case _ if lst.isEmpty => str + "]"
          case _ if lst.tail.isEmpty => printAllElements(str + lst.head, lst.tail)
          case _ => printAllElements(str + lst.head + ", ", lst.tail)
      }
      printAllElements("[", this)
    }
  }

  //Obiekt MyList
  object MyList {
    def apply(): MyList[Nothing] = new MyEmptyList
    def apply[A](args: A*): MyList[A] = args.tail match {
        case _ if args.tail.isEmpty => new MyNonEmptyList(args.head, new MyEmptyList)
        case _ => new MyNonEmptyList(args.head, apply(args.tail: _*))
    }
  }
}
