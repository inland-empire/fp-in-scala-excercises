def factorial(n:Int): Int = {
  @annotation.tailrec
  def go(n:Int, acc:Int): Int =
    if (n<=0) acc
    else go(n-1, n*acc)
  go(n, 1)  
}

def fib(n:Int): Int= {
     |   if(n<=0) 0
     |   else if (n ==1)  1
     |   else fib1(n-2) + fib1(n-1)
     | }

def isSorted[A] (as: Array[A],ordered:(A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n:Int): Boolean =
    if ( n>= as.length-1) true
    else if ( !ordered(as(n+1), as(n)) ) false
    else loop(n+1)
   
  loop(1)
}

def curry[A,B,C] (f:(A,B) => C): A=> (B=>C) = {
     |     a => b => f(a,b)
}

def uncurry[A,B,C](f:A=>B=>C): (A,B)=>C = {
     |   (a,b) => f(a)(b)
     | }

def compose[A,B,C](f:B=>C, g:A=>B): A=>C = 
     |   (a:A) => f(g(a))
