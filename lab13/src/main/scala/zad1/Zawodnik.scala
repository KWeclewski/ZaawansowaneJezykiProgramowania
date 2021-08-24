package zad1

import akka.actor.Actor

object Zawodnik {
  case object Próba
  // polecenie wykonania próby (kończy się zwróceniem Wyniku,
  // za pomocą komunikatu Grupa.Wynik)
}

class Zawodnik extends Actor {
  def receive: Receive = {
    case msg => println(msg)
  }
}
