object Main extends App {
  println("Jeśli koniecznie chcemy, to w Scali możemy też pisać w stylu XIX-wiecznym:")
  print("Podaj wartość y: ")
  // Wczytujemy liczbę całkowitą i zapamiętujemy ją jako „wartość” max
  // Użycie wartości zamiast zmiennej to drobne odstępstwo od stylu XIX-wiecznego ;)
  val max = io.StdIn.readInt()
  // deklarujemy i inicjalizujemy zmienną x
  var x = 1
  // drobna próbka „interpolacji” w wypisywanym napisie
  println(s"Liczby z przedziału [1..$max] to:")
  // w „pętli” zmieniamy wartość zmiennej aż będzie ona równa „max”
  while (x <= max) {
    println(x)
    x += 1
  }
}
