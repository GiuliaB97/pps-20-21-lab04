package u04lab.code

import u04lab.code.Lists._
import u04lab.code.Optionals._

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A])
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

case class PowerIteratorsFactoryImpl() extends PowerIteratorsFactory {
  private case class PowerIteratorAbs() extends PowerIterator[Int]{
    override def next(): Option[Int] = ???

    override def allSoFar(): List[Int] = ???

    override def reversed(): PowerIterator[Int] = ???
  }
  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = ???

  override def fromList[A](list: List[A]): Unit = ???

  override def randomBooleans(size: Int): PowerIterator[Boolean] = ???
}



