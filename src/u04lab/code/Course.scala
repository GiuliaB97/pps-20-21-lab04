package u04lab.code

class Course {
  trait Course {

    def name: String
    def teacher: String

    // a template method
    override def toString(): String = name + " " + teacher + " "
  }
  case class CaseCourse(name:String, teacher:String)
}
