import akka.actor._

//zad 1
case class Wstaw(n: Int)
case class Znajdź(n: Int)

class Węzeł extends Actor {

  def liść(wartość: Int): Receive = {
    case Wstaw(n) =>
      println(wartość, n)

      if (n<wartość) {
        val lewe= context.actorOf(Props[Węzeł], name=s"$n")
        lewe ! Wstaw(n)
        context.become(zLewymPoddrzewem(lewe= lewe, wartość=wartość ))
      } else if (n>wartość) {
        val prawe = context.actorOf(Props[Węzeł], name=s"$n")
        prawe ! Wstaw(n)
        context.become(zPrawymPoddrzewem(wartość, prawe))
      }
      else {
        println(s"JA ${self.path.name} - istnieje juz")
      }
    case Znajdź(n) =>
      println(s"Szukam $n")
      if (n<wartość) {println("NIE MA GO TU")}
      else if (n>wartość) {println("NIE MA GO TU")}
      else {println(s"JA ${self.path.name} - istenije juz tu")}
  }

  def zLewymPoddrzewem(lewe: ActorRef, wartość: Int): Receive = {
    case Wstaw(n) =>
      if (n<wartość) {
        lewe ! Wstaw(n)
      }
      else if (n>wartość) {
        val prawe= context.actorOf(Props[Węzeł], name= s"$n")
        prawe ! Wstaw(n)
        context.become(zPoddrzewami(lewe, wartość, prawe))
        println(s"Tworze węzeł ${wartość} z PODDRZEWAMI ${lewe.path.name}, ${prawe.path.name}")
      }
      else {
        println(s"JA ${self.path.name} - istnieje juz")
      }
    case Znajdź(n) =>
      println(s"Szukam $n")
      if (n>wartość) {println("NIE MA GO TU")}
      else if  (n<wartość) {lewe ! Znajdź(n)}
      else {println(s"JA ${self.path.name} - istenije juz tu")}
  }

  def zPrawymPoddrzewem(wartość: Int, prawe: ActorRef): Receive = {
    case Wstaw(n) =>
      if (n>wartość) {
        prawe ! Wstaw(n)
      }
      else if (n<wartość) {
        val lewe= context.actorOf(Props[Węzeł], name= s"$n")
        lewe ! Wstaw(n)
        context.become(zPoddrzewami(lewe, wartość, prawe))
        println(s"Tworze węzeł ${wartość} z PODDRZEWAMI ${lewe.path.name}, ${prawe.path.name}")
      }
      else {
        println(s"JA ${self.path.name} - istnieje juz")
      }
    case Znajdź(n) =>
      println(s"$n -szukam tego")
      if (n>wartość) {prawe ! Znajdź(n)}
      else  if (n<wartość) {println("NIE MA GO TU")}
      else {println(s"JA ${self.path.name} - istenije juz tu")}
  }

  def zPoddrzewami(lewe: ActorRef, wartość: Int, prawe: ActorRef): Receive = {
    case Wstaw(n) =>
      if (n < wartość) {
        lewe ! Wstaw(n)
      }
      else if (n > wartość) {
        prawe ! Wstaw(n)
      }
      else {
        println(s"Ja ${self.path.name} - istnieje juz")
      }

    case Znajdź(n) =>
      println(s"$n szukam tego")
      if (n<wartość) {lewe ! Znajdź(n)}
      else if (n>wartość) {prawe ! Znajdź(n)}
      else {println(s"JA ${self.path.name} - istenije juz tu")}
  }

  def receive: Receive = {
    case Wstaw(n) =>
      println(s"Wstawiam lisc o wartosci $n")
      context.become(liść(n))
    case Znajdź(n) =>
      println(s"Brak węzłów, nie mam w czym szukać $n")
  }
}

object lab12_a extends App {
  val system = ActorSystem("system")
  val wezel = system.actorOf(Props[Węzeł], "wezel")

  wezel ! Wstaw(44)
  wezel ! Wstaw(12)
  wezel ! Wstaw(10)
  wezel ! Wstaw(14)
  wezel ! Wstaw(11)
  wezel ! Wstaw(13)
  wezel ! Wstaw(15)

  //  wezel ! Wstaw(11)
  //  wezel ! Wstaw(13)
  //
  //  wezel ! Znajdź(80)
}