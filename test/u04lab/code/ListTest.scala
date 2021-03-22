package u04lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u04lab.code.Lists.List.{Cons, Nil, appendByFold, filterByFlatmap, length}

class ListTest {
  @Test
  def testFilterByFlatmap(): Unit = {
    val v1 = filterByFlatmap(Cons(10, Cons(20, Nil())))(_>15)
    val v2 = filterByFlatmap(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)
    assertEquals(v1, Cons(20, Nil()))
    assertEquals(v2, Cons("a",Cons("bb", Nil())))
  }

  @Test
  def testAppendByFold(): Unit = {
    val v1 = appendByFold(Cons(3,Cons(7,Nil())), Cons(1,Cons(5,Nil())))
    val v2 = appendByFold(Cons(3,Nil()), Cons(9,Nil()))
    assertEquals(v1, Cons(3,Cons(7,Cons(1,Cons(5, Nil())))))
    assertEquals(v2, Cons(3,Cons(9, Nil())))
  }

  @Test
  def testLength(): Unit = {
    val v1 = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    val v2 = Nil()
    assertEquals(length(v1), 4)
    assertEquals(length(v2), 0)
  }
}
