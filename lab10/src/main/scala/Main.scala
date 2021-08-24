import Main.sys
//TODO Do dokonczenia

import scala.io._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

//zad1 cz.1
object Player {
  case class Ping(i: Int)
  case class Pong(i: Int)
  case class Play(a: ActorRef, maks: Int)
}
class Player extends Actor {
  import Player._
  def receive: Receive = {
    case Play(a, i) =>
      println(s"${self.path.name}: Zaczynam grać z $a")
      a ! Ping(i-1)
    case Ping(i) =>
      println(s"${self.path.name}: Dostałem Ping $i")
      i match {
        case 0 =>
          context.system.terminate()
        case _ =>
          sender() ! Pong(i-1)
      }
    case Pong(i) =>
      println(s"${self.path.name}: Dostałem Pong $i")
      i match {
        case 0 =>
          context.system.terminate()
        case _ =>
          sender() ! Ping(i-1)
      }
  }
}
object Main extends App {
  import Player._
  val sys = ActorSystem("sys")
  val addrOfJohn = sys.actorOf(Props[Player](), "John")
  val addrOfKate = sys.actorOf(Props[Player](), "Kate")

  addrOfJohn ! Play(addrOfKate, 10)

  println("-" * 50 + "Naciśnij ENTER żeby zakończyć" + "-" * 50)
  StdIn.readLine()
  sys.terminate()


//zad1 cz.2
object Player {
  case class Ping(i: Int)
  case class Pong(i: Int)
  case class Play(a: List[ActorRef], maksOkrazen: Int)
}
class Player extends Actor {
  import Player._
  def receive: Receive = {
    case Play(a, i) =>
      println(s"${self.path.name}: Zaczynam grać z $a")
      println(List(a) ::: List(sender()))
    case Ping(i) =>
      println(s"${self.path.name}: Dostałem Ping $i")
      i match {
        case 0 =>
          context.system.terminate()
        case _ =>
          sender() ! Pong(i-1)
      }
    case Pong(i) =>
      println(s"${self.path.name}: Dostałem Pong $i")
      i match {
        case 0 =>
          context.system.terminate()
        case _ =>
          sender() ! Ping(i-1)
      }
  }
}
object Main extends App {
  import Player._
  val sys = ActorSystem("sys")
  val addrOfJohn = sys.actorOf(Props[Player](), "John")
  val addrOfKate = sys.actorOf(Props[Player](), "Kate")
  val addrOfPaul = sys.actorOf(Props[Player](), "Paul")
  val addrOfChristy = sys.actorOf(Props[Player](), "Christy")

  addrOfJohn ! Play(List(addrOfKate,addrOfChristy,addrOfPaul), 10)

  println("-" * 50 + "Naciśnij ENTER żeby zakończyć" + "-" * 50)
  StdIn.readLine()
  sys.terminate()
}