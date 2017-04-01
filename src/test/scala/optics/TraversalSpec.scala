package optics

import optics.TraversalSpec.Name
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{Assertion, FreeSpec}
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
          Laws.headOption(t)
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
          Laws.modifyGetAll(t)
        }
      }

      "set idempotent" - {
        "by example" in {
          val name = Name("Emily", "Smith", "Milly")
          t.set("Emilia", t.set("Emilia", name)) shouldBe t.set("Emilia", name)
        }

        "property based" in {
          Laws.setIdempotent(t)
        }
      }

      "modify identity" - {
        "property based" in {
          Laws.modifyIdentity(t)
        }
      }

      "compose modify" - {
        "property based" in {
          Laws.composeModify(t)
        }
      }
    }
  }

  "traversal from lenses" - {
    val t = Traversal.unsafeFromLenses(List(Name.firstL, Name.nicknameL))
    "basics" - {
      "by example" in {
        t.getAll(Name("Emily", "Smith", "Milly")) shouldBe List("Emily", "Milly")
        t.set("Amelia", Name("Emily", "Smith", "Milly")) shouldBe Name("Amelia", "Smith", "Amelia")
        t.modify(_.toUpperCase)(Name("Emily", "Smith", "Milly")) shouldBe Name("EMILY", "Smith", "MILLY")
        t.headOption(Name("Emily", "Smith", "Milly")) shouldBe Some("Emily")
      }
      "property based" in {
        forAll(arbitrary[Name]){name =>
          t.getAll(name) shouldBe List(name.first, name.nickname)
        }
        forAll(arbitrary[String], arbitrary[Name]){(string, name) =>
          t.set(string, name) shouldBe Name(string, name.last, string)
        }
        forAll(arbitrary[String => String], arbitrary[Name]){(f, name) =>
          t.modify(f)(name) shouldBe Name(f(name.first), name.last, f(name.nickname))
        }
        forAll(arbitrary[Name]){name =>
          t.headOption(name) shouldBe Some(name.first)
        }
      }
    }

    "laws" in {
      Laws.headOption(t)
      Laws.modifyGetAll(t)
      Laws.setIdempotent(t)
      Laws.modifyIdentity(t)
      Laws.composeModify(t)
    }
  }

  object Laws {
    def headOption[S: Arbitrary, A](t: Traversal[S, A]): Assertion =
      forAll(arbitrary[S]) { s =>
        t.headOption(s) shouldBe t.getAll(s).headOption
      }

    def modifyGetAll[S, A](t: Traversal[S, A])(implicit arbAtoA: Arbitrary[A => A], arbS: Arbitrary[S]): Assertion =
      forAll(arbitrary[A => A], arbitrary[S]) { (f, s) =>
        t.getAll(
          t.modify(f)(s)
        ) shouldBe t.getAll(s).map(f)
      }

    def setIdempotent[S: Arbitrary, A: Arbitrary](t: Traversal[S, A]): Assertion =
      forAll(arbitrary[A], arbitrary[S]) { (a, s) =>
        t.set(a, t.set(a, s)) shouldBe t.set(a, s)
      }

    def modifyIdentity[S: Arbitrary, A](t: Traversal[S, A]): Assertion =
      forAll(arbitrary[S]) { s =>
        t.modify(identity[A])(s) shouldBe s
      }

    def composeModify[S, A](t: Traversal[S, A])(implicit arbAtoA: Arbitrary[A => A], arbS: Arbitrary[S]): Assertion =
      forAll(arbitrary[A => A], arbitrary[A => A], arbitrary[S]) { (f, g, s) =>
        t.modify(g)(t.modify(f)(s)) shouldBe t.modify(g compose f)(s)
      }
  }
}

object TraversalSpec {
  case class Name(first: String, last: String, nickname: String)
  object Name {
    val nicksAreImmortal: Traversal[Name, String] = Traversal.apply2[Name, String](_.first, _.last){ (f, l, n) => n.copy(first = f, last =  l)}

    val firstL: Lens[Name, String] = Lens[Name, String](_.first)((f, n) => n.copy(first = f))
    val nicknameL: Lens[Name, String] = Lens[Name, String](_.nickname)((nick, name) => name.copy(nickname = nick))
  }
}
