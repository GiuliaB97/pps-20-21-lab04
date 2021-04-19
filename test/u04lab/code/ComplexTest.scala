package u04lab.code

import org.junit.jupiter.api.Assertions.{assertEquals, assertNotEquals}
import org.junit.jupiter.api.Test

class ComplexTest {
  private val a=Array(Complex(10,20), Complex(1,1), Complex(7,0));
  private val c1 = a(0) + a(1) + a(2)
  private val c2 = a(0) * a(1)
  private val newComplexNumberImpl= new ComplexImpl(18, 21)
  private val newCaseComplexNumber= Complex(18, 21)

  @Test
  def testCreateComplex(): Unit = {
    assertEquals(Complex(18.0,21.0), c1)
    assertEquals(Complex2(18.0,21.0), newComplexNumberImpl)
    assertEquals(Complex(18.0,21.0), newCaseComplexNumber)
    assertEquals(Complex(-10.0,30.0), c2)
  }

  @Test
  def testSumComplex(): Unit = {
    assertEquals(Complex(18.0,21.0)+Complex(-10.0,30.0), c1+c2)
    assertEquals(Complex2(18.0,21.0)+Complex2(18.0,21.0), newComplexNumberImpl+newComplexNumberImpl )
    assertEquals(Complex(18.0,21.0)+Complex2(18.0,21.0), c1+newComplexNumberImpl )
  }

  @Test
  def testProductComplex(): Unit = {
    assertEquals(Complex(18.0,21.0)*Complex(-10.0,30.0), c1*c2)
    assertEquals(Complex2(18.0,21.0)*Complex2(18.0,21.0), newComplexNumberImpl*newComplexNumberImpl )
    assertEquals(Complex(18.0,21.0)*Complex2(18.0,21.0), c1*newComplexNumberImpl )
  }

  @Test
  def testEquality(): Unit = {
    assertEquals(c1, c1)
    assertEquals(c2, c2)
    assertNotEquals(c1, c2)
    assertNotEquals(Complex2(18.0,21.0), c1)
    assertEquals(Complex2(18.0,21.0), newComplexNumberImpl)
    assertEquals(Complex(18.0,21.0), c1)
  }

  @Test def testToString(): Unit = {
    assertEquals("CaseComplex(1.0,1.0)", Complex(1, 1).toString)               //non funziona
    assertEquals("ComplexImpl(1.0, 1.0)", Complex2(1, 1).toString)
  }
}
