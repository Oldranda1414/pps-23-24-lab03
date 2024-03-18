package u03

import extensionmethods.Streams.Stream
import extensionmethods.Streams.Stream.*

object TakeWhile:
    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
        case _ => Empty()
    
        