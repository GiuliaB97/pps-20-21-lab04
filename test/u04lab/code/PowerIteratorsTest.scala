package u04lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u04lab.code.Lists._
import u04lab.code.Optionals._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()); // elementi già prodotti
    for (i <- 0 until 10) {
      pi.next(); // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test
  def testRandom() {
    val pi = factory.randomBooleans(2);

    val b1 = Option.getOrElse(pi.next(),false)
    val b2 = Option.getOrElse(pi.next(),false)
    assertTrue(Option.isEmpty(pi.next()))
    assertEquals(List.Cons(b1, List.Cons(b2, List.Nil())), pi.allSoFar());
  }

  @Test
  def testFromList() {
    val pi = factory.fromList(List.Cons(1, List.Cons(2, List.Nil())));

    assertEquals(pi.next, Option.of(1))
    assertEquals(pi.allSoFar, List.Cons(1, List.Cons("b",List.Nil())))
    assertEquals(pi.next, Option.of(2))
    assertEquals(pi.allSoFar, List.Cons(1, List.Cons(2,List.Nil())))
    assertTrue(Option.isEmpty(pi.next))
  }
}