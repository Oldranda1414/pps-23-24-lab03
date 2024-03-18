package u03

import u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence
import org.junit.* 
import org.junit.Assert.*
import u03.Courses.*

class CoursesTest:
    import Person.*
    import Sequences.Sequence.*

    val year: Int = 2023

    val l: Sequence[Person] = Cons(Student("Mario", year), Cons(Teacher("Ricci", "pcd"), Cons(Student("Rossi", year), Cons(Teacher("Viroli", "pps"), Nil()))))

    @Test def testCourses() =
        val expected: Sequence[String] = Cons("pcd",Cons("pps", Nil()))
        assertEquals(expected, courses(l))
        assertEquals(expected, l.coursesExtension)