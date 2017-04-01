package optics

import optics.TraversalSpec.Name
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

class TraversalSpec extends FreeSpec with GeneratorDrivenPropertyChecks {

  implicit val arbName = Arbitrary(for{
    first <- arbitrary[String]
    last <- arbitrary[String]
    nickname <- arbitrary[String]
  } yield Name(first, last, nickname))

  "traversal" - {
    val t = Name.nicksAreImmortal
    "basics" - {
      "by example" in {
        t.getAll(Name("Emily", "Smith", "Milly")) shouldBe List("Emily", "Smith")
        t.set("Amelia", Name("Emily", "Smith", "Milly")) shouldBe Name("Amelia", "Amelia", "Milly")
        t.modify(_.toUpperCase)(Name("Emily", "Smith", "Milly")) shouldBe Name("EMILY", "SMITH", "Milly")
        t.headOption(Name("Emily", "Smith", "Milly")) shouldBe Some("Emily")
      }

      "property based" in {
        forAll(arbitrary[Name]){name =>
          t.getAll(name) shouldBe List(name.first, name.last)
        }
        forAll(arbitrary[String], arbitrary[Name]){(string, name) =>
          t.set(string, name) shouldBe Name(string, string, name.nickname)
        }
        forAll(arbitrary[String => String], arbitrary[Name]){(f, name) =>
          t.modify(f)(name) shouldBe Name(f(name.first), f(name.last), name.nickname)
        }
        forAll(arbitrary[Name]){name =>
          t.headOption(name) shouldBe Some(name.first)
        }
      }
    }

    "laws" - {
      "headOption" - {
        "by example" in {
          val name = Name("Emily", "Smith", "Milly")
          t.headOption(name) shouldBe t.getAll(name).headOption
        }

        "property based" in {
          forAll(arbitrary[Name]){name =>
            t.headOption(name) shouldBe t.getAll(name).headOption
          }
        }
      }

      "modify get all" - {
        "by example" in {
          val name = Name("Emily", "Smith", "Milly")
          t.getAll(
            t.modify(_.toUpperCase)(name)
          ) shouldBe t.getAll(name).map(_.toUpperCase)
        }

        "property based" in {
          forAll(arbitrary[String => String], arbitrary[Name]){(f, name) =>
            t.getAll(
              t.modify(f)(name)
            ) shouldBe t.getAll(name).map(f)
          }
        }
      }

      "set idempotent" - {
        "by example" in {
          val name = Name("Emily", "Smith", "Milly")
          t.set("Emilia", t.set("Emilia", name)) shouldBe t.set("Emilia", name)
        }

        "property based" in {
          forAll(arbitrary[String], arbitrary[Name]){(string, name) =>
            t.set(string, t.set(string, name)) shouldBe t.set(string, name)
          }
        }
      }

      "modify identity" - {
        "property based" in {
          forAll(arbitrary[Name]){name =>
            t.modify(identity[String])(name) shouldBe name
          }
        }
      }

      "compose modify" - {
        "property based" in {
          forAll(arbitrary[String => String], arbitrary[String => String], arbitrary[Name]){(f, g, name) =>
            t.modify(g)(t.modify(f)(name)) shouldBe t.modify(g compose f)(name)
          }
        }
      }
    }
  }
}

object TraversalSpec {
  case class Name(first: String, last: String, nickname: String)
  object Name {
    val nicksAreImmortal: Traversal[Name, String] = Traversal.apply2[Name, String](_.first, _.last){ (f, l, n) => n.copy(first = f, last =  l)}
  }
}
