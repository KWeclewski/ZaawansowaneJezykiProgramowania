import akka.actor.{ActorSystem, Actor, ActorRef, Props}

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wynik(wynik: Int)
case class Wykonaj(a: String)

class Nadzorca extends Actor {
  def receive: Receive = {
    case Init(liczbaPracownikow) =>
      var lista: List[ActorRef] = List()
      for (n <- 1 to liczbaPracownikow) {
        lista = context.actorOf(Props[Pracownik], s"pracownik$n") :: lista
      }
      println(s"Stworzono $liczbaPracownikow Pracowników")
      context.become(wysylaZlecenia(lista))
  }

  def wysylaZlecenia(lista: List[ActorRef]):Receive = {
    case Zlecenie(tekst) =>
      var index = 0
      tekst.foreach(a => {
        lista(index) ! Wykonaj(a)
        println(s"Wysłano zlecenie pracownikowi ${lista(index).path}")
        if (index == lista.length - 1) {
          index = 0
        } else {
          index = index + 1
        }
      })
      context.become(zliczSlowa(0, 0, tekst))
  }

  def zliczSlowa(suma:Int, elementy:Int, tekst: List[String]):Receive = {
    case Wynik(wynik) =>
      if (elementy == tekst.length-1) {
        println(s"Liczba słów: ${wynik + suma}")
        context.become(receive)}
      else {
        context.become(zliczSlowa(suma+wynik, elementy+1, tekst))
      }
  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Wykonaj(a) =>
      println(s"${self.path} otrzymał zlecenie" )
      a match{
        case "" =>
          sender() ! Wynik(0)
        case _ =>
          sender() ! Wynik(a.split(' ').toList.length)
      }
  }
}

object lab11_1 extends App {
  val dane= scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
  val system = ActorSystem("systemBiuro")
  val nadzorca = system.actorOf(Props[Nadzorca], "Nadzorca")
  nadzorca ! Init(5)
  nadzorca ! Zlecenie(dane)
}