package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  
  import FunSets._

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    
    def setFromNonEmptyList(numbers: List[Int]):Set = 
      if (numbers.tail.isEmpty)
        singletonSet(numbers.head)
      else
        union(singletonSet(numbers.head), setFromNonEmptyList(numbers.tail))    
        
    val s_odd   = setFromNonEmptyList(List(1,3,5,7,9,11,13))
    val s_even  = setFromNonEmptyList(List(2,4,6,8,10,12,14))
    val s_fib   = setFromNonEmptyList(List(1,2,3,5,8,13,21))
    val s_large = setFromNonEmptyList(List(-1000, -750, -500, -250, 1000, 750, 500, 250, 0))
    
    
    def odd(x:Int) = (x%2)==1
    def even(x:Int) = (x%2)==0
    def plus1(x:Int) = x+1
    def minus1(x:Int) = x-1

  }
  
  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  
  
  test("singletonSet(1) contains 1") {    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
      new TestSets {
          /**
           * The string argument of "assert" is a message that is printed in case
           * the test fails. This helps identifying which assertion failed.
           */
          assert(contains(s1, 1), "Singleton")
      }
  }

  test("union contains all elements") {
      new TestSets {
          val s = union(s1, s2)
                  assert(contains(s, 1), "Union 1")
                  assert(contains(s, 2), "Union 2")
                  assert(!contains(s, 3), "Union 3")
      }
  }
  
  test("union self") {
    new TestSets {
      val s = union(s1,s1)
              assert(contains(s, 1), "Union 1")
              assert(!contains(s, 2), "Union 2")
              assert(!contains(s, 3), "Union 3")
    }
  }

  test("union multi") {
      new TestSets {
          val s = union(s_odd,s_even)
                  assert(contains(s, 1), "odd 1")
                  assert(contains(s, 3), "odd 3")
                  assert(contains(s, 8), "even 8")
                  assert(!contains(s, 0), "0")
      }
  }
  
  test("intersect - none") {
      new TestSets {
          val s = intersect(s_odd,s_even)
                  assert(!contains(s, 1), "odd 1")
                  assert(!contains(s, 3), "odd 3")
                  assert(!contains(s, 8), "even 8")
                  assert(!contains(s, 0), "0")
      }
  }

  test("intersect - some") {
      new TestSets {
          val s = intersect(s_odd,s_fib)
          assert(contains(s, 1), "odd fib 1")
          assert(!contains(s, 2), "fib 3")
          assert(contains(s, 5), "odd fib 5")
          assert(!contains(s, 7), "odd 7")
          assert(!contains(s, 8), "fib 8")
          assert(contains(s, 13), "odd fib 3")
      }
  }

  test("diff") {
      new TestSets {
          val s = diff(s_even,s_fib)
          assert(contains(s, 6), "6")
          assert(contains(s, 10), "10")
          assert(contains(s, 14), "14")
          assert(!contains(s, 2), "2")
          assert(!contains(s, 8), "8")
          assert(!contains(s, 13), "13")
          assert(!contains(s, 99), "99")

      }
  }

  test("exists 1") {
      new TestSets {
        assert(exists(s_even,singletonSet(4)), "even 4")
        assert(exists(s_even,singletonSet(10)), "even 10")
        assert(!exists(s_odd,singletonSet(10)), "odd 10")
        assert(!exists(s_even,singletonSet(3)), "even 3")
        assert(exists(s_odd,singletonSet(3)), "odd 3")
      }
  }

  test("exists 2") {
      new TestSets {
          assert(exists(s_even,(x:Int) => x == 4))
          assert(!exists(s_even,(x:Int) => x == 3))
      }
  }
  
  test("forall") {
      new TestSets {
          assert(forall(s_even, even), "even")
          assert(forall(s_odd, odd), "odd")
      }
  }

  test("map") {
      new TestSets {
          assert(forall(map(s_even, plus1), odd), "even+1=odd")
          assert(forall(map(s_odd,  plus1), even), "odd+1=even")
      }
  }
  
  test("filter") {
      new TestSets {
          val even_fibs = filter(s_fib,even)

          assert(exists(s_fib,odd), "some fibs are odd");
          assert(!forall(s_fib,even), "not all fibs are even")

          assert(!exists(even_fibs, odd), "no even fib is odd");
          assert(forall(even_fibs,even), "all even fibs are even")
      }
  }  
  
  test("bounds") {
    new TestSets {
       val x  = setFromNonEmptyList(List(1,3,4,5,7,1000))       
       val xminus1   = map(x, minus1)
       val expected = setFromNonEmptyList(List(0,2,3,4,6,999))
       
       assert(forall(xminus1, expected), "bounds")
    }
    
  }
}
