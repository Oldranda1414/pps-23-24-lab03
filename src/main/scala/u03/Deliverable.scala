package u03

import Sequences.Sequence
import Sequences.Sequence.*

object Deliverable:

    //Task 1a
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(head, tail) if n > 0=> Cons(head, take(tail)(n-1))
      case _  => Nil()

    //Task 1b
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Cons(h1, t1) => second match
        case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()
      case _ => Nil()
    
    //Task 1c
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(head, tail) => Cons(head, concat(tail, l2))
      case _ => l2 match
        case Cons(head, tail) => Cons(head, tail)
        case _ => Nil()
      
    //Task 1c
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(head, tail) => concat(mapper(head), flatMap(tail)(mapper))
      case _ => Nil()
    
    //Task 1d
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap(l1)(v => pred(v) match
        case true => Cons(v, Nil())
        case _ => Nil()
      )

    
    //Task 2
    import u03.Optionals.Optional
    import u03.Optionals.Optional.*

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(head, tail) => min(tail) match
        case Optional.Just(x) if x < head => Optional.Just(x)
        case _ => Optional.Just(head)
      case _ => Optional.Empty()

    //Task 3

    import u02.AlgebraicDataTypes.Person
    import u02.AlgebraicDataTypes.Person.*

    def courses(l: Sequence[Person]): Sequence[String] = 
        flatMap(l)(_ match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )

    //Task 4
    def foldLeft[A](l: Sequence[A])(counter: A)(f: (A, A) => A): A = l match
      case Sequence.Cons(head, tail) => foldLeft(tail)(f(counter, head))(f)
      case _ => counter

    //Task 5
    extension (l: Sequence[Person]) def coursesExtension: Sequence[String] = 
        flatMap(l)(_ match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )

    extension [A](l: Sequence[A]) 
        def foldLeftExtension(counter: A)(f: (A, A) => A): A = l match
            case Sequence.Cons(head, tail) => foldLeft(tail)(f(counter, head))(f)
            case _ => counter

    //Task 6
    import u03.extensionmethods.Streams.Stream
    import u03.extensionmethods.Streams.Stream.*

    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
        case _ => Stream.Empty()
    
        
    //Task 7
    def fill[A](n: Int)(item: A): Stream[A] = n match
        case n if n > 0 => Cons(() => item, () => fill(n - 1)(item))
        case _ => Stream.Empty()

    //Task 8
    import math.*

    def pell(): Stream[Int] = 
        def findPell(i: Int): Double = (pow((1 + sqrt(2)), i) - pow((1 - sqrt(2)), i)) / (2 * sqrt(2)) 
    
        val s = iterate(0)(_ + 1)
        s.map(i => round(round(findPell(i))))
