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


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
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

  test("multiple union contains all elements") {
    new TestSets {
      val s = union(union(union(s1, s2), s3), s4)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
      assert(contains(s, 4), "Union 4")
    }
  }

  test("intersects contain common elements") {
    new TestSets {
      val i1 = union(union(s1, s3), s4)
      val i2 = union(union(s1, s2), s4)

      val i = intersect(i1, i2)

      assert(contains(i, 1), "Intersect 1")
      assert(!contains(i, 2), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
      assert(contains(i, 4), "Intersect 4")
    }
  }

  test("diff contains difference") {
    new TestSets {
      val d1 = union(union(s1, s3), s4)
      val d2 = union(union(s1, s2), s4)

      val d = diff(d1, d2)

      assert(!contains(d, 1), "Diff 1")
      assert(contains(d, 3), "Diff 3")
      assert(!contains(d, 4), "Diff 4")
    }
  }

  test("filter contain appropriate elements") {
    new TestSets {
      val f1 = union(union(union(s1, s2), s3), s4)

      val f = filter(f1, x => x % 2 == 0)

      assert(!contains(f, 1), "Intersect 1")
      assert(contains(f, 2), "Intersect 2")
      assert(!contains(f, 3), "Intersect 3")
      assert(contains(f, 4), "Intersect 4")
    }
  }

  test("forall even") {
    new TestSets {
      val a1 = union(union(s2, s4), s6)
      assert(forall(a1, (x => x % 2 == 0)), "evens")

      val a2 = union(union(s2, s3), s6)
      assert(!forall(a2, (x => x % 2 == 0)), "evens")
    }
  }

  test("forall odd") {
    new TestSets {
      val a1 = union(union(s1, s3), s5)
      assert(forall(a1, (x => x % 2 != 0)), "odds")

      val a2 = union(union(s1, s2), s5)
      assert(!forall(a2, (x => x % 2 != 0)), "odds")
    }
  }

  test("exists even") {
    new TestSets {
      val e1 = union(union(s1, s2), s3)
      assert(exists(e1, (x => x % 2 == 0)), "exists even")

      val e2 = union(union(s1, s3), s5)
      assert(!exists(e2, (x => x % 2 == 0)), "exists even")
    }
  }

  test("map 2x") {
    new TestSets {
      val s = union(union(union(s1, s2), s3), s4)
      val dbl = map(s, (x => 2 * x))
      assert(!contains(dbl, 1), "?x2=1")
      assert(contains(dbl, 2), "1x2=2")
      assert(!contains(dbl, 3), "?x2=3")
      assert(contains(dbl, 4), "2x2=4")
      assert(!contains(dbl, 5), "?x2=5")
      assert(contains(dbl, 6), "3x2=6")
      assert(!contains(dbl, 7), "?x2=7")
      assert(contains(dbl, 8), "4x2=8")
      assert(!contains(dbl, 9), "?x2=9")
      assert(!contains(dbl, 10), "5x2=10")
    }
  }
}
