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
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("Testing singletonSet") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton(1) contains returned false for 1")
      assert(!contains(s2, 1), "Singleton(2) contains return true for 1")
      assert(!contains(s3, 2), "Singleton (3) contains return true for 2")
      assert(contains(s2, 2), "Singleton(2) contains returned false for 2")
      assert(contains(s3, 3), "Singleton (3) contains returned false for 3")
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

  test("intersect contains only shared elements") {
    new TestSets {
      val s = union(s1, s2)
      val is = intersect(s, s2)
      assert(!contains(is, 1), "intersect 1 contains should be false")
      assert(contains(s, 2), "intersect 2 contains should be true")
    }
  }

  test("diff contains elements on first set but not in second") {
    new TestSets {
      val s = union(s1, s2)
      val is = union(s, s3)
      val ds = diff(s, s1)
      assert(contains(ds, 2), "2 should be part of diff set")
      assert(!contains(ds, 1), "1 should not be part of diff set")
    }
  }

  test("filter subset of `s` for which `p` holds") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(filter(s, { p: Int => { p % 2 == 0 } }), 2), "2 should be in the set")
      assert(!contains(filter(s, { p: Int => { p % 2 == 0 } }), 1), "2 should be in the set")
    }
  }

  test("forall which tests whether all bounded integers within `s` satisfy `p`.") {
    new TestSets {
      val s4 = singletonSet(4)
      val s11 = union(s4, s2)
      val s21 = union(s3, s11)
      assert(forall(s11, { p: Int => { p % 2 == 0 } }), "all elements in the set should be even")
      assert(!forall(s21, { p: Int => { p % 2 == 0 } }), "all elements in the set should not be even")
    }
  }

  test("exists which tests whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      val s4 = singletonSet(4)
      val s11 = union(s4, s2)
      val s21 = union(s3, s11)
      assert(exists(s11, { p: Int => { p % 2 == 0 } }), "there is atleast one even number")
      assert(exists(s21, { p: Int => { p % 2 == 0 } }), "there is atleast one even number")
      assert(!exists(s21, { p: Int => { p % 5 == 0 } }), "there is no element divisible by 5")
    }
  }

  test("map which returns a set transformed by applying `f` to each element of `s`.") {
    new TestSets {
//      val o = map(union(singletonSet(4), s2), { p: Int => { p * 3 } })
      val b = union(union(singletonSet(1), singletonSet(3)), singletonSet(4))
      val c = union(union(singletonSet(5), singletonSet(7)), singletonSet(1000))
      val d = map(union(b, c), { p: Int => { p * 3 } })

//      assert(contains(o, 6), "transformed map should contain 6")
//      assert(contains(o, 12), "transformed map should contain 24")
//      assert(!contains(o, 25), "transformed map should not contain 25")
      assert(contains(d, 3000), "transformed map should not contain 25")
    }
  }
}
