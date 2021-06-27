package u04lab.code

import u04lab.code.Lists._
import u04lab.code.Optionals._
import u04lab.code.Streams.Stream
import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

object PowerIterator {
  def apply[A](stream: Stream[A]): PowerIterator[A] = new PowerIteratorImpl(stream) //apply class is for free just w/ case class

  private case class PowerIteratorImpl[A](var inputStream: Stream[A]) extends PowerIterator[A] {
    private var actualList: List[A] = List.Nil()

    override def next(): Option[A] = inputStream match {
      case  Stream.Cons(h, t) => {                                            //if it is not the of the stream
        actualList = List.append(actualList, List.Cons[A](h(), List.Nil()))   //then the local list must be updated w/ the next value
        inputStream = t()                                                     //next the stream variable must be updated to be just the tail; in order to not iterate over the same value always and for ever
        Option.Some(h())                                                      //finally the actual value must give back to the caller
      }
      case _ => Option.None()                                                 //in any other case return an Option of None
    }

    override def allSoFar(): List[A] = actualList                             //the values read until that moment should already been added to the local variable
    override def reversed(): PowerIterator[A] = new PowerIteratorsFactoryImpl().fromList(List.reverse(actualList))
  }
}
class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {
  override def incremental(start: Int, next: Int => Int): PowerIterator[Int] = PowerIterator[Int](Stream.iterate(start)(next))
  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIterator[A](List.toStream(list))
  override def randomBooleans(size: Int): PowerIterator[Boolean] = {
    val random:Random=new Random()
    PowerIterator[Boolean](Stream.take(Stream.generate(random.nextBoolean()))(size))
  }
}


