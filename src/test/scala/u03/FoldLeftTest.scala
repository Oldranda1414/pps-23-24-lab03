package u03

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence
import FoldLeft.*


class FoldLeftTest:
    import Sequence.*

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))


    @Test def testFoldLeft() = 
        val stringSeq: Sequence[String] = Cons("Hello, ", Cons("World", Cons("!", Nil())))

        assertEquals(70, foldLeft(l)(10)(_ + _))
        assertEquals(-60, foldLeft(l)(0)(_ - _))
        assertEquals("Hello, World!", foldLeft(stringSeq)("")(_ + _))

        assertEquals(70, l.foldLeftExtension(10)(_ + _))
        assertEquals(-60, l.foldLeftExtension(0)(_ - _))
        assertEquals("Hello, World!", foldLeft(stringSeq)("")(_ + _))