package u03

import extensionmethods.Streams.Stream
import extensionmethods.Streams.Stream.*

object StreamFunctions:
    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
        case _ => Empty()
    
        
    def fill[A](n: Int)(item: A): Stream[A] = n match
        case n if n > 0 => Cons(() => item, () => fill(n - 1)(item))
        case _ => Empty()
    