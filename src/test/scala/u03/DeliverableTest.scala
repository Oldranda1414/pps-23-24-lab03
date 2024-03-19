package u03

import org.junit.* 
import org.junit.Assert.*

import Deliverable.*

//Task 1 and 2 tests where already present

class Task1Test:

    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.Cons
    import u03.Sequences.Sequence.Nil

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
        assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

    @Test def testFilter() =
        assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
        assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
    
    @Test def testTake() =
        assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
        assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
        assertEquals(Nil(), take(l)(0))
        assertEquals(Nil(), take(Nil())(2))
    
    @Test def testZip() = 
        val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
        assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
        assertEquals(Nil(), zip(l, Nil()))
        assertEquals(Nil(), zip(Nil(), l2))
        assertEquals(Nil(), zip(Nil(), Nil()))

    @Test def testConcat() =
        val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
        assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
        assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

    @Test def testFlatMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
        assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

class Task2Test:

    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u03.Optionals.Optional
    import u03.Optionals.Optional.*

    @Test def testMin() =
        val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

        assertEquals(Just(10), min(l))
        assertEquals(Just(1), min(Cons(1, Nil())))
        assertEquals(Empty(), min(Nil()))

class Task3Test:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u02.AlgebraicDataTypes.Person
    import u02.AlgebraicDataTypes.Person.*

    @Test def testCourses() =
        val year: Int = 2023
        val l: Sequence[Person] = Cons(Student("Mario", year), Cons(Teacher("Ricci", "pcd"), Cons(Student("Rossi", year), Cons(Teacher("Viroli", "pps"), Nil()))))
        val expected: Sequence[String] = Cons("pcd",Cons("pps", Nil()))

        assertEquals(expected, courses(l))

class Task4Test:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*

    @Test def testFoldLeft() =
        val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
        val stringSeq: Sequence[String] = Cons("Hello, ", Cons("World", Cons("!", Nil())))
        
        assertEquals(70, foldLeft(l)(10)(_ + _))
        assertEquals(-60, foldLeft(l)(0)(_ - _))
        assertEquals("Hello, World!", foldLeft(stringSeq)("")(_ + _))

class Task5Test:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u02.AlgebraicDataTypes.Person
    import u02.AlgebraicDataTypes.Person.*

    @Test def testCoursesExtension() =
        val year: Int = 2023
        val l: Sequence[Person] = Cons(Student("Mario", year), Cons(Teacher("Ricci", "pcd"), Cons(Student("Rossi", year), Cons(Teacher("Viroli", "pps"), Nil()))))
        val expected: Sequence[String] = Cons("pcd",Cons("pps", Nil()))

        assertEquals(expected, l.coursesExtension)

    @Test def testFoldLeftExtension =
        val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
        val stringSeq: Sequence[String] = Cons("Hello, ", Cons("World", Cons("!", Nil())))

        assertEquals(70, l.foldLeftExtension(10)(_ + _))
        assertEquals(-60, l.foldLeftExtension(0)(_ - _))
        assertEquals("Hello, World!", foldLeft(stringSeq)("")(_ + _))

class Task6Test:
    import u03.extensionmethods.Sequences.Sequence.*
    import u03.extensionmethods.Streams.Stream

    @Test def takeWhileTest() =
        val s = Stream.iterate(0)(_ + 1)
        val actual = Stream.toList(takeWhile(s)(_ < 4)) 
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), actual)

class Task7Test:
    import u03.extensionmethods.Sequences.Sequence.*
    import u03.extensionmethods.Streams.Stream

    @Test def fillTest() =
        val expected = Cons("a", Cons("a", Cons("a", Nil())))
        val actual = Stream.toList(fill(3)("a"))
        assertEquals(expected, actual)

class Task8Test:
    import u03.extensionmethods.Sequences.Sequence.*
    import u03.extensionmethods.Streams.Stream.toList
    import u03.extensionmethods.Streams.Stream.take

    @Test def pellTest() =
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), toList(take(pell())(5)))