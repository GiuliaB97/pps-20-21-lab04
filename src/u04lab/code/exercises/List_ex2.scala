package u04lab.code.exercises

import scala.annotation.tailrec

object List_ex2 extends App {
    // A generic linkedlist
    sealed trait List[E]

    // a companion object (i.e., module) for List
    object List {
      case class Cons[E](head: E, tail: List[E]) extends List[E]
      case class Nil[E]() extends List[E]

      def nil[A]: List[A] = Nil() // smart constructor
      //Done
      def sum(l: List[Int]): Int = l match{
        case Cons(h,t) => h+sum(t)
        case _=> 0
      }

      def drop[A](l: List[A], n: Int): List[A] = l match{
        case Cons(_,t) if(n>0) => drop(t, n-1)
        case _ => l
      }
      def contain[A](l: List[A], e:A): Boolean = l match{
        case Cons(h,_) if e==h => true
        case Cons(_, t)=> contain(t, e)
        case Nil()=> false
      }

      def length(l: List[_]): Int = l match{
        case Cons(h, t)=> 1+ length(t)
        case _=> 0
      }

      def append[A](l1: List[A], l2: List[A]): List[A]= (l1, l2) match{
        case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1, append(t1,Cons(h2,t2)))
        case (Nil(), Cons(_, _))=> l2
        case (Cons(_, _), Nil())=>l1
        case (Nil(), Nil())=> Nil()

      }
      def map[A,B](l: List[A])(f: A => B): List[B] = l match{
        case Cons(h,t)=> Cons(f(h), map(t) (f))
        case _=> Nil()
      }

      def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match{
        case Cons(h,t)=> append(f(h), flatMap(t)(f))
        case _ => Nil()

      }
      def filter[A](l: List[A])(f: A => Boolean): List[A] = l match{
        case Cons(h,t) if f(h)=> Cons(h, filter(t) (f))
        case Cons(_,t) => filter(t)(f)
        case _=> Nil ()
      }

      @tailrec
      def foldLeft[A,B](l: List[A])(acc: B)(f: (B,A)=>B): B = l match{
        case Cons(h,t)=> foldLeft(t)(f(acc, h))(f)
        case Nil()=> acc
      }

      def foldRightNonTailRec[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B = l match{
        case Cons(h,t)=> f(h, foldRightNonTailRec(t)(acc)(f))
        case Nil()=> acc
      }

      def reverse[A](l: List[A]) : List[A] = foldLeft(l:List[A])(Nil():List[A]) ((acc, head)=>Cons(head, acc))

      def reverse2[A](l: List[A]) : List[A] = foldRightNonTailRec(l:List[A])(Nil():List[A])((head, acc) => Cons(head, acc ))

      def foldRightViaFoldleft[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B = foldLeft(reverse(l))(acc)((acc,h)=> f(h, acc))

      def foldRight[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B = foldRightViaFoldleft(l)(acc)(f)

      //TO-DO
      def filterByFlatmap[A](l: List[A])(f: A => Boolean): List[A] = ???

      def appendByFold[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1)(l2)((h, t)=>Cons(h,t))

    }


    // Note "List." qualification
    //println(List.sum(List.Cons(10, List.Cons(20, List.Cons(30, List.Nil()))))) // 60
    import List._
        println("APPEND: \"10\",\"1\",\"2\"    "+append(Cons("10", Nil()), Cons("1", Cons("2", Nil())))) // "10","1","2"

        println("DROP:Cons(30, Nil())   "+drop(Cons(10, Cons(20, Cons(30, Nil()))),2)) // Cons(30, Nil())
        println("DROP: Nil()   "+drop(Cons(10, Cons(20, Cons(30, Nil()))),5)) // Nil()
        println("DROP: Nil()   "+drop(Nil(), 5)) // Nil()

        println("MAP: Cons(11, Cons(21, Nil()))   "+map(Cons(10, Cons(20, Nil())))(_+1))       // Cons(11, Cons(21, Nil()))
        println("MAP: Cons(\":10:\", Cons(\":20:\",Nil()))   "+map(Cons(10, Cons(20, Nil())))(":"+_+":")) // Cons(":10:", Cons(":20:",Nil()))


        println("FILTER : Cons(20, Nil())      "+filter(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
        println("FILTER : Cons(20, Nil())       "+filter(Cons(10, Cons(20, Cons(20, Nil()))))(_>15)) // Cons(20, Nil())
        println("FILTER: Cons(\"a\",Cons(\"bb\", Nil()))      "+filter(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))


        val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
        println("FOLDLEFT:-16 "+foldLeft(lst)(0)(_-_)) // -16
        println("FOLDRIGHT:8 "+foldRightNonTailRec(lst)(0)(_-_)) // -8
        println("FOLDRIGHT via FOLDLEFT: -8 "+foldRightViaFoldleft(lst)(0)(_-_)) // -8
        println("REVERSE Cons(5, COns(1, (COns(7, Cons(3, Nil())))))      "+reverse(lst))
        println("REVERSE 2  Cons(5, COns(1, (COns(7, Cons(3, Nil())))))      "+reverse2(lst))
          // EXERCISES:
          /*
          println(filterByFlatmap(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
          println(filterByFlatmap(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))
          */

          println("APPEND BY FOLD"+appendByFold(Cons(3,Cons(7,Nil())), Cons(1,Cons(5,Nil())))) // Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
          println("APPEND BY FOLD"+appendByFold(Nil(), Cons(1,Cons(5,Nil())))) // Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
          println("APPEND BY FOLD"+appendByFold(Cons(3,Cons(7,Nil())),Nil())) // Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))


          println(length(Nil())) // 0
          println(length(Cons(3,Cons(7,Cons(1,Cons(5, Nil())))))) // 4



    //online ex

    //println(zipWith(Cons("a",Cons( "b",(Cons("c", Nil())))), Cons("A",Cons( "B",(Cons("C", Nil())))))(_ + _) )

  }