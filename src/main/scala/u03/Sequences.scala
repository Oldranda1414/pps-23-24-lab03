package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Streams.Stream.empty

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap(l1)(v => pred(v) match
        case true => Cons(v, Nil())
        case _ => Nil()
      )

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Cons(h1, t1) => second match
        case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()
      case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(head, tail) if n > 0=> Cons(head, take(tail)(n-1))
      case _  => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(head, tail) => Cons(head, concat(tail, l2))
      case _ => l2 match
        case Cons(head, tail) => Cons(head, tail)
        case _ => Nil()
      
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(head, tail) => concat(mapper(head), flatMap(tail)(mapper))
      case _ => Nil()
    

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(head, tail) => min(tail) match
        case Optional.Just(x) if x < head => Optional.Just(x)
        case _ => Optional.Just(head)
      case _ => Optional.Empty()

    def foldLeft[A](l: Sequence[A])(counter: A)(f: (A, A) => A): A = l match
      case Cons(head, tail) => foldLeft(tail)(f(counter, head))(f)
      case _ => counter
    
      
    
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
