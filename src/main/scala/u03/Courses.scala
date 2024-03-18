package u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.*

object Courses:

    def courses(l: Sequence[Person]): Sequence[String] = 
        flatMap(l)(_ match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )


    extension (l: Sequence[Person]) def coursesExtension: Sequence[String] = 
        flatMap(l)(_ match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )