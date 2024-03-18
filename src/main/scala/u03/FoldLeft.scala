package u03

import u03.Sequences.Sequence


object FoldLeft:
    def foldLeft[A](l: Sequence[A])(counter: A)(f: (A, A) => A): A = l match
      case Sequence.Cons(head, tail) => foldLeft(tail)(f(counter, head))(f)
      case _ => counter


    extension [A](l: Sequence[A]) 
        def foldLeftExtension(counter: A)(f: (A, A) => A): A = l match
            case Sequence.Cons(head, tail) => foldLeft(tail)(f(counter, head))(f)
            case _ => counter