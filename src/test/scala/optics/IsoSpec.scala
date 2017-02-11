package optics

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

case class FamilyName(text: String)
object FamilyName {
  val textI: Iso[FamilyName, String] = Iso[FamilyName, String](_.text)(FamilyName.apply)
}

case class Person(familyName: FamilyName)
object Person {
  val familyNameI: Iso[Person, FamilyName] = Iso[Person, FamilyName](_.familyName)(Person.apply)
}

case class Entry(name: String, value: String)
object Entry {
  val asTupleI: Iso[Entry, (String, String)] =
    Iso[Entry, (String, String)](e => (e.name, e.value)){case (n, v) => Entry(n, v)}
}

class IsoSpec extends FreeSpec with GeneratorDrivenPropertyChecks {
  private val familyNameGen: Gen[FamilyName] = for {
    s ← arbitrary[String]
  } yield FamilyName(s)

  private val personGen: Gen[Person] = for {
    fn ← familyNameGen
  } yield Person(fn)

  "Iso" - {
    "basics" - {
      "by example (FamilyName <-> String)" in {
        FamilyName.textI.get(FamilyName("Smith")) shouldBe "Smith"
        FamilyName.textI.reverseGet("Smith") shouldBe FamilyName("Smith")
      }

      "by example (Entry <-> tuple)" in {
        Entry.asTupleI.get(Entry("id", "0021")) shouldBe ("id", "0021")
        Entry.asTupleI.reverseGet(("id", "0021")) shouldBe Entry("id", "0021")
      }

      "property based" in {
        forAll{(s: String) => FamilyName.textI.get(FamilyName(s)) shouldBe s}
        forAll{(s: String) => FamilyName.textI.reverseGet(s) shouldBe FamilyName(s)}
      }
    }

    "properties" - {
      "by example" in {
        FamilyName.textI.reverseGet(FamilyName.textI.get(FamilyName("Smith"))) shouldBe FamilyName("Smith")
        FamilyName.textI.get(FamilyName.textI.reverseGet("Smith")) shouldBe "Smith"
      }

      "property based" in {
        forAll(familyNameGen){ fn => FamilyName.textI.reverseGet(FamilyName.textI.get(fn)) shouldBe fn }
        forAll{(s: String) => FamilyName.textI.get(FamilyName.textI.reverseGet(s)) shouldBe s}
      }
    }

    "modify" in {
      val marriage: FamilyName => FamilyName = FamilyName.textI.modify(_ + "-Jones")

      val name = FamilyName("Smith")
      marriage(name) shouldBe FamilyName("Smith-Jones")
    }

    "set" in {
      val marriage: FamilyName => FamilyName = FamilyName.textI.set("Jones")

      val name = FamilyName("Smith")
      marriage(name) shouldBe FamilyName("Jones")
    }

    "iso compose iso => iso" - {
      val composed: Iso[Person, String] = Person.familyNameI compose FamilyName.textI

      "basics" in {
        val person = Person(FamilyName("Smith"))
        composed.set("Jones")(person) shouldBe Person(FamilyName("Jones"))

        // as a comparison, how to do the same using each level's copy method
        person.copy(familyName = person.familyName.copy(text = "Jones")) shouldBe Person(FamilyName("Jones"))
      }

      "composed satisfies properties" in {
        forAll(personGen) { person => composed.reverseGet(composed.get(person)) shouldBe person }
        forAll{(s: String) => composed.get(composed.reverseGet(s)) shouldBe s}
      }
    }
  }
}
