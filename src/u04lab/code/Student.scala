package u04lab.code

import u04lab.code.Lists.List.{Cons, contain, map}
import u04lab.code.Lists._

import scala.annotation.tailrec // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(course: Course): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String
  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)
  private case class StudentImpl(name: String, year: Int) extends Student{
    private var _courses: List[Course] = List.Nil()
    override def enrolling(course: Course): Unit = _courses = Cons( course , _courses )

    override def courses: List[String] = map(_courses)(_.name)
    override def hasTeacher(teacher: String): Boolean = contain(map(_courses)(_.teacher), teacher)
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)
  private case class CourseImpl(name: String, teacher: String) extends Course
}

object sameTeacher {
  def sameTeacher(courses: List[Course]): Option[String] = {
    val teachers: List[String] = map(courses)(course => course.teacher)
    var prev: String=""

    @tailrec
    def _sameTeacher(list: List[String]): Option[String] = list match { //non puoi passare prev come argomento della funzione perchè di default sono val
      case Cons(h, t) => if(prev=="") prev=h; _sameTeacher(t)
      case Cons(h, t) => if (prev==h) _sameTeacher(t) else None
      case _ =>  Some(prev)
    }
    _sameTeacher(teachers)
  }
}

object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017

  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)

  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
}

/** Hints:
 * - simply implement Course, e.g. with a case class
 * - implement Student with a StudentImpl keeping a private Set of courses
 * - try to implement in StudentImpl method courses with map
 * - try to implement in StudentImpl method hasTeacher with map and find
 * - check that the two println above work correctly
 * - refactor the code so that method enrolling accepts a variable argument Course*
 */