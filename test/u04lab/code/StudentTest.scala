package u04lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class StudentTest {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017

  @Test
  def testCreateCourse(): Unit = {
    assertEquals(cPPS, Course("PPS","Viroli"))
    assertEquals(cPCD, Course("PCD","Ricci"))
    assertEquals(cSDR, Course("SDR","D'Angelo"))
  }

  @Test
  def testCreateStudent(): Unit = {
    assertEquals(s1, Student("mario",2015))
    assertEquals(s2, Student("gino",2016))
    assertEquals(s3, Student("rino",2017))//test default value
  }

  @Test
  def testEnrolling(): Unit = {
    s1.enrolling(cPPS)
    //assertEquals(s1, Cons(PCD,Nil()))
    s1.enrolling(cPCD)
    //assertEquals(s1, Cons(PCD,Cons(PPS,Nil())))
    s2.enrolling(cPPS)
    //assertEquals(s2, Cons(PPS,Nil()))
    s3.enrolling(cPPS)
    //assertEquals(s3, Cons(PPS,Nil()))
    s3.enrolling(cPCD)
    //assertEquals(s3, Cons(PPS,Cons(PCD,Nil())))
    s3.enrolling(cSDR)
    //assertEquals(s3,Cons(SDR,Cons(PCD,Cons(PPS,Nil())))
  }

  @Test
  def testHasTeacher(): Unit = {
    assertEquals(s1.hasTeacher("Ricci"), true) // true
    assertEquals(s1.hasTeacher("D'Angelo"), false)
    assertEquals(s2.hasTeacher("Viroli"), true)
    assertEquals(s2.hasTeacher("Ricci"), false)
    assertEquals(s3.hasTeacher("Viroli"), true)
    assertEquals(s3.hasTeacher("Bravetti"), false)
  }

}
