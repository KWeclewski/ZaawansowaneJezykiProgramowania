object Main extends App {
  //zad 1
  println("Podaj dwie liczby")
  println("Podaj liczbe 1: ")
  var x = io.StdIn.readInt()
  println("Podaj liczbe 2: ")
  var y = io.StdIn.readInt()

    while(x != y){
     if (x > y){
        x = x - y;
      }
      else {
        y = y - x;
      }
    }

    println(s"NWD to $x") 
    
    //zad 2
  print("Podaj wartość : ")
  var a = io.StdIn.readInt()
  var bool : Boolean = true
   
  if (a < 2){
    bool = false
  }
  var b = 0
  b = a / 2
  for(i <- 2 to b ){
    if (a % i == 0){
      bool = false
    }
  }
  
  
  if (bool == false){
    print("Nie jest liczba pierwsza")
  }
  else{
    print("Jest liczba pierwsza")
  }
}
