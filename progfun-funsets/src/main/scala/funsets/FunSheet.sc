package funsets

object FunSheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import FunSets._
  
  def setFromNonEmptyList(numbers: List[Int]):Set =
      if (numbers.tail.isEmpty)
        singletonSet(numbers.head)
      else
        union(singletonSet(numbers.head), setFromNonEmptyList(numbers.tail))
                                                  //> setFromNonEmptyList: (numbers: List[Int])Int => Boolean
        
  val s_odd   = setFromNonEmptyList(List(1,3,5,7,9,11,13))
                                                  //> s_odd  : Int => Boolean = <function1>
  val s_even  = setFromNonEmptyList(List(2,4,6,8,10,12,14))
                                                  //> s_even  : Int => Boolean = <function1>
  val s_fib   = setFromNonEmptyList(List(1,2,3,5,8,13,21))
                                                  //> s_fib  : Int => Boolean = <function1>
  val s_large = setFromNonEmptyList(List(-1000, -750, -500, -250, 1000, 750, 500, 250, 0))
                                                  //> s_large  : Int => Boolean = <function1>
    

    
  FunSets.toString(s_fib)                         //> res0: String = {1,2,3,5,8,13,21}
  
  FunSets.toString(map(s_fib, _ * 2))             //> res1: String = {2,4,6,10,16,26,42}
  
}