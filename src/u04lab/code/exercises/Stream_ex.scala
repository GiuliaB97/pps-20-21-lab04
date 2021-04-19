package u04lab.code.exercises

import scala.util.Random

object Stream_ex extends App {
  import u04lab.code.Lists._
  sealed trait Stream[A]

  object Stream {
    case class Empty[A]() extends Stream[A]
    case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    //NON CHIARO QUANDO USARE List.Cons, o cons
    def toList[A](stream: Stream[A]): List[A] = stream match{
      case Cons(h,t)=> List.Cons(h(), toList(t()))
      case _ => List.Nil()
    }

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match{
      case Cons(h,t)=> cons(f(h()), map(t())(f) )
      case _ => Empty()
    }

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match{
      case Cons(h, t) if pred(h())=>  cons(h(), filter(t())(pred))
      case Cons(_, t)=> filter(t())(pred)
      case _=> Empty()
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = stream match{
      case Cons(h,t) if(n>0)=> cons(h(), take(t())(n-1))
      case _=> Empty()
    }

    def takeWhile[A](stream: Stream[A])(pred: A=>Boolean): Stream[A] = stream match{
      case Cons(h, t) if pred(h())=> cons(h(), takeWhile(t())(pred))
      case _=> Empty()
    }

    def fold[A,B](stream: Stream[A])(base: =>B)(op: (A,B)=>B): B = ???

    def peek[A](stream: Stream[A])(exec: A=>Unit): Stream[A] = ???

    def iterate[A](init: => A)(next: A => A): Stream[A] = ???

    def generate[A](next: => A): Stream[A] = ???

  }

  import Stream._
  val n = Random.nextInt(100)
  var str = generate(scala.io.StdIn.readLine())
  var str2 = map(str)(Integer.parseInt(_))
  str2 = takeWhile(str2)(_!=n)
  var str3 = map(str2)({case num if (num>n) => "higher"; case _ => "lower"})
  str3 = peek(str3)(s=>println("yours is: "+s))
  println("you won in "+fold(str3)(0)((a,b)=>b+1)+" steps")

  /*
  // var simplifies chaining of functions a bit..
  var str = Stream.iterate(0)(_+1)   // {0,1,2,3,..}
  str = Stream.map(str)(_+1)    // {1,2,3,4,..}
  str = Stream.filter(str)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  str = Stream.take(str)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str)) // [1,2,21,22,..,28]

  val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
*/

}
