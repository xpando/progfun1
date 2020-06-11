package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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

    val s4 = union(s1, union(s2, s3))
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `singleton set does not contain two`: Unit = {
    new TestSets {
      assert(!contains(s1, 2))
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains only elements in both sets`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      val s = intersect(s4, s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  @Test def `diff contains difference of two sets`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      val s = diff(s4, s1)
      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(contains(s, 3), "Diff 3")
    }
  }

  @Test def `filter contains elements of first set that satisfy condition`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      val s = filter(s4, (x: Int) => x != 1)
      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
      assert(!contains(s, -1), "Filter 4")
    }
  }

  @Test def `forall evaluates correctly`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      assert(forall(s4, (x: Int) => x > 0), "forall x > 0")
      assert(forall(union(s4, singletonSet(-bound-1)), (x: Int) => x > 0), "forall x > 0 with out of bounds element -bound-1")
      assert(!forall(s4, (x: Int) => x < 0), "!forall x < 0")
      assert(!forall(union(s4, singletonSet(bound+1)), (x: Int) => x < 0), "!forall x < 0 with out of bounds element bound+1")
      assert(!forall(s4, (x: Int) => x > 2), "!forall x > 2")
    }
  }

  // this test is using my initial attempt at implementing exists, doesn't use forall
  @Test def `exists evaluates correctly`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      assert(exists(s4, (x: Int) => x > 0), "exists x > 0")
      assert(exists(s4, (x: Int) => x > 2), "exists x > 2")
      assert(!exists(singletonSet(bound+1), (x: Int) => x > 0), "!exists x > 0 with out of bounds element bound+1")
      assert(!exists(s4, (x: Int) => x < 0), "!exists x < 0")
      assert(!exists(union(s4, singletonSet(-bound-1)), (x: Int) => x < 0), "!exists x < 0 with out of bounds element -bound-1")
    }
  }

  @Test def `map evaluates correctly`: Unit = {
    new TestSets {
      // s4 = [1,2,3]
      // mapSet = [1,4,9]
      val mapSet = map(s4, x => scala.math.pow(x,2).toInt)

      assert(contains(mapSet, 1), "Map 1")
      assert(!contains(mapSet, 2), "Map 2")
      assert(contains(mapSet, 4), "Map 3")
      assert(contains(mapSet, 9), "Map 4")
      assert(!contains(mapSet, 10), "Map 5")
      assert(!contains(mapSet, 0), "Map 6")
      assert(!contains(mapSet, -1), "Map 7")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
