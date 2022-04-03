package u05lab.ex1

import org.junit.Test
import org.junit.Assert.{assertEquals, assertThrows}
import u05lab.ex1.List

class ListTest {
  val l: List[Int] = List(10, 20, 30, 40, 50)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((10, 0), (20, 1), (30, 2), (40, 3), (50, 4)), l.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(30, 40, 50), List(10, 20)), l.partition(_ > 20))

  @Test
  def testPartition2(): Unit =
    assertEquals((List(30, 40, 50), List(10, 20)), l.partition2(_ > 20))

  @Test
  def testSpan(): Unit =
    assertEquals((List.Nil(), List(10, 20, 30, 40, 50)), l.span(_ > 30))
    assertEquals((List(10), List(20, 30, 40, 50)), l.span(_ < 20))

  def testReduce(): Unit =
    assertEquals((List.Nil(), List(10, 20, 30, 40, 50)), l.span(_ > 30))

  def testTakeRight(): Unit =
    assertEquals(List(20,30,40,50), l.takeRight(30))

  def testCollect() : Unit =
    assertEquals(List(9,39), l.collect{ case x if x<15 || x>30 => x-1 })

  //def testCollect(): Unit =
}