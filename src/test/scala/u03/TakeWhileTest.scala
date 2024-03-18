package u03

import org.junit.* 
import org.junit.Assert.*
import u03.Sequences.Sequence
import u03.Streams.Stream

class TakeWhileTest:
    import Sequences.Sequence.*
    import u03.Streams.Stream.*

    val s = Stream.iterate(0)(_ + 1)

    @Test def takeWhileTest() =
        val actual = Stream.toList(Stream.takeWhile(s)(_ < 4)) 
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), actual)