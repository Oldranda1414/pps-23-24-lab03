package u03

import org.junit.* 
import org.junit.Assert.*
import u03.extensionmethods.Sequences
import u03.extensionmethods.Streams.Stream
import u03.extensionmethods.Streams.Stream.*
import u03.StreamFunctions.* 

class StreamFunctionsTest:
    import Sequences.Sequence.*


    @Test def takeWhileTest() =
        val s = Stream.iterate(0)(_ + 1)
        val actual = Stream.toList(takeWhile(s)(_ < 4)) 
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), actual)

    @Test def fillTest() =
        val expected = Cons("a", Cons("a", Cons("a", Nil())))
        val actual = Stream.toList(fill(3)("a"))
        assertEquals(expected, actual)