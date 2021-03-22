package u04lab.code

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

class ComplexImpl(  override val re: Double,
                    override val im: Double
                 )  extends Complex {
  override def +(c: Complex): Complex = Complex (re + c.re, im + c.im)

  override def *(c: Complex): Complex = Complex ((re * c.re)-(im * c.im), (re*c.im)+(im * c.re))


  def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexImpl]

  override def equals(other: Any): Boolean = other match {
    case that: ComplexImpl =>
      (that canEqual this) &&
        re == that.re &&
        im == that.im
    case _ => false
  }

  override def toString = s"ComplexImpl($re, $im)"

  override def hashCode(): Int = {
    val state = Seq(re, im)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Complex {
  def apply(re:Double, im:Double):Complex = CaseComplex(re, im)
  private case class CaseComplex(re: Double, im: Double) extends Complex {
    override def +(c: Complex): Complex = Complex (re + c.re, im + c.im)
    override def *(c: Complex): Complex = Complex ((re * c.re)-(im * c.im), (re * c.im)+(im * c.re))
  }
}

object Complex2 {
  def apply(re:Double, im:Double):Complex = new ComplexImpl(re, im)
}


object TryComplex extends App {
  val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
  }

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */