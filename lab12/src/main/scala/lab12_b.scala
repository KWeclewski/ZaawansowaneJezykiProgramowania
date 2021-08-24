import akka.actor._

//zad 2
case class Wstaw2(n: Int)
case class Usuń(n: Int)
case class Pusto(n: Int)

case object SprawdzStan

class Element extends Actor {

  def receive: Receive = {
    case Wstaw2(n) =>
      context.become(korzeń(n))
      println(s"Jestem korzeń root$n")
  }

  def korzeń(wartość: Int): Receive = {
    case Wstaw2(n) =>
      if (wartość == n) {
        println(s"Tworzę pierwszego ${n}_0 potomka i wysyłam Wstaw do zPotomkami")
        val potomek = context.actorOf(Props[Element], s"${n}_0")
        potomek ! Wstaw2(n)
        context.become(zPotomkami(wartość, Set(potomek)))
      }
    case Usuń(n) =>
      if (wartość == n) {
        println(s"Chce usunąć korzeń ${n}, bo nie ma potomków do usunięcia")
        sender() ! Pusto(n)
        self ! PoisonPill
      }
  }

  def zPotomkami(wartość: Int, kolejny: Set[ActorRef]): Receive = {
    case Wstaw2(n) =>
      if (wartość == n) {
        val potomek = context.actorOf(Props[Element], s"${n}_${kolejny.size}")
        potomek ! Wstaw2(n)
        val dodaj= potomek :: kolejny.toList
        context.become(zPotomkami(wartość, (dodaj.reverse).toSet))
        print(s"- potomkowie korzenia $n \n${dodaj.foreach(potomek => print(potomek.path.name+ " "))}")
      }
    case Usuń(n) =>
      if (wartość == n) {
        println(s"Chce usunąć potomka z korzenia $n")
        kolejny.head ! PoisonPill
        if (kolejny.tail.isEmpty) {
          println(s"ostatni potomek- ${kolejny.head}")
        } else {
          println(s"- pozostali potomkowie korzenia $n \n${kolejny.tail.foreach(potomek => print(potomek.path.name + " "))}")
        }
        context.become(zPotomkami(wartość, kolejny.tail))
      }
  }

}

class Nadzorca extends Actor {
  def receive: Receive = {
    case SprawdzStan =>
      println(s"nazwa nadzorcy jego adres ${self.path.name}")
      context.become(stan(Set()))
      println("sprawdz stan poczatkowy")
  }

  def stan(listy: Set[Int]): Receive = {
    case Wstaw2(n) =>
      println(s"Chcę wstawić $n")
      if (listy.contains(n)) {
        println(s"${self.path.name}: mam już korzeń $n, chcę dodać do niego potomka ")
        context.actorSelection(s"/user/Nadzrca/root${n}") ! Wstaw2(n)
      } else {
        val nowyKorzen = context.actorOf(Props[Element], s"root${n}")
        nowyKorzen ! Wstaw2(n)
        println(s"${self.path.name} nie mam jeszcze korzenia-> tworze korzeń ${nowyKorzen.path.name} i dodaje do 'listy'")
        context.become(stan((n::listy.toList).toSet))
      }
    case Usuń(n) =>
      if (listy.contains(n)) {
        println(s"Chce usunąć element $n")
        context.actorSelection(s"/user/Nadzorca/root${n}") ! Usuń(n)
      }
      else {
        println(s"Nie znaleziono elementów $n")
      }
    case Pusto(n) =>
      val nowaLista= listy.filter(_ != n)
      context.become(stan(nowaLista))
  }
}

object lab12_b extends App {
  val system = ActorSystem("systemWorkowLiczb")

  val nadzorca = system.actorOf(Props[Nadzorca], "Nadzorca" )

  nadzorca ! SprawdzStan
  nadzorca ! Wstaw2(3)
  nadzorca ! Wstaw2(3)
  nadzorca ! Wstaw2(3)
  nadzorca ! Wstaw2(5)
  nadzorca ! Wstaw2(5)
  nadzorca ! Wstaw2(5)
  nadzorca ! Wstaw2(9)
  nadzorca ! Wstaw2(9)

  nadzorca ! Usuń(7)

}