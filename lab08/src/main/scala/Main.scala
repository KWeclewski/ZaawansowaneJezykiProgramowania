object Main extends App {
  class C(r: Double, i: Double) {
    val x = r
    val i = i

    override def toString = i match {
      case 0 => "Liczba zespolona: "+x
      case _ if i > 0 => "Liczba zespolona: "+x+"+"+i+"i"
      case _ if i < 0 => "Liczba zespolona: "+x+"-"+i.abs+"i"
    }

    def this(a: Double) = this(a, 0)

    def +(that: C) = new C(x+that.x, i+that.i)
    def -(that: C) = new C(x-that.x, i-that.i)
    def *(that: C) = new C((x*that.x)-(i*that.i), (i*that.x)-(x*that.i))
    def /(that: C) = {
      require((that.x*that.x)+(that.i*that.i) != 0)
      new C((x*that.x)-(i*that.i)/((that.x*that.x)+(that.i*that.i)), (i*that.x)-(x*that.i)/((that.x*that.x)+(that.i*that.i)))
    }

    implicit def /(d: Double) = {
      require(d != 0)
      new C(x/d, i/d)
    }
  }

  object C {
    def apply(x: Double, i: Double) = new C(x, i)
    def apply(x: Double) = new C(x)

    implicit def fromDouble(d: Double) = new C(d)
  }
}
