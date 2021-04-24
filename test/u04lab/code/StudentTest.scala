package u04lab.code

import org.junit.jupiter.api.Assertions.{assertEquals, assertNotEquals}
import org.junit.jupiter.api.Test
import u04lab.code.Lists.List.{Cons, Nil}
class StudentTest {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val cSEIOT =Course("SEIOT","RICCI")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cSDR)
  val list1 = Cons(cPCD, Cons(cSEIOT, Nil()))
  val list2 = Cons(cPCD, Cons(cPPS, Nil()))
  val list3 = List(cPCD, cSEIOT)

  @Test
  def testCreateCourse(): Unit = {
    assertEquals(cPPS, Course("PPS","Viroli"))
    assertEquals(cPCD, Course("PCD","Ricci"))
    assertEquals(cSDR, Course("SDR","D'Angelo"))
    assertEquals("SDR", cSDR.name)
    assertEquals("D'Angelo", cSDR.teacher)
  }

  @Test
  def testCreateStudent(): Unit = {
    assertEquals(s1, Student("mario",2015))
    assertEquals(s2, Student("gino",2016))
    assertEquals(s3, Student("rino"))//test default value
    assertEquals(s3.name, "rino")
    assertEquals(s3.year, 2017)
  }

  @Test
  def testEnrolling(): Unit = {
    assertEquals(Cons("PCD", Cons("PPS", Nil())), s1.courses)
    assertEquals( Cons("SDR", Nil()), s2.courses)
    assertEquals(Nil(), s3.courses)
  }

  @Test
  def testHasTeacher(): Unit = {
    assertEquals(s1.hasTeacher("Ricci"), true)
    assertEquals(s1.hasTeacher("D'Angelo"), false)
    assertEquals(s2.hasTeacher("Viroli"), false)
    assertEquals(s2.hasTeacher("D'Angelo"), true)
    assertEquals(s3.hasTeacher("Viroli"), false)
    assertEquals(s3.hasTeacher("Bravetti"), false)
  }

  @Test
  def testExtractor(): Unit ={
    assertEquals(Option("Ricci"), sameTeacher.sameTeacher(list1))
    assertNotEquals(Option.empty, sameTeacher.sameTeacher(list2))
    //assertEquals(Option("Ricci"), sameTeacher.sameTeacher(list3))
  }
}
