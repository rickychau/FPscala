object Ex1_to_5 {
  def main(args: Array[String]) = {

    //Ex.1
    for(i <- 1 to 100)
      println("no." + i +":" + fib(i))



    //Ex.2
    //val arr = Array(1,2,3,4,5,6,7)
    val arr2: Array[Int] = Array(7,6,5,4,3,2,1)
    val sorted = isSorted(arr2, greaterThan)
    if (sorted) {
      println("True")
    }
    else {
      println("False")
    }

  }


  //Ex.1
  def fib(n: BigInt): BigInt = {
    def go(n: BigInt, n_minus1: BigInt, n_minus2: BigInt): BigInt = {
      if (n == BigInt(0)) n_minus2
      else go (n - 1, n_minus1 + n_minus2, n_minus1)
    }
    if (n == BigInt(1)) 0
    else if (n == BigInt(2)) 1
    else go(n - 1 , 1, 0)
  }

  //Ex.2
  def greaterThan(x: Int, y: Int): Boolean = {
    if (x > y) true
    else false
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1 || as.length == 0) true
      else if (gt(as(n),as(n+1))) loop(n+1)
      else false
    loop(0)
  }

  //Ex.3

  def curry[A, B, C](f:(A,B) => C): A => (B => C) = {

    (a: A) => (b: B) => f(a, b)
  }

  //Ex.4

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //Ex.5
  def compose[A, B, C](f: B => C, g: A => B): A => C ={
    (a: A ) => f(g(a))
  }

}
