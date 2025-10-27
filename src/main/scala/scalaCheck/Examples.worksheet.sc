import $dep. `org.scalacheck::scalacheck:1.19.0`

import org.scalacheck.Prop.forAll
import org.scalacheck._

val propReverseList = forAll { (lst: List[String]) => lst.reverse.reverse == lst}
val propConcatString = forAll { (s1: String, s2: String) => (s1 + s2).endsWith(s2) }

propReverseList.check()

val smallInteger = Gen.choose(0, 100)

val propSmallInteger = Prop.forAll(smallInteger)( n => n >= 0 && n <= 100 )

propSmallInteger.check()