package optics

import optics.Nationality.{british, swiss}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

case class Nationality(text: String)
object Nationality {
  val textI: Iso[Nationality, String] = Iso[Nationality, String](_.text)(Nationality.apply)

  val british = Nationality("British")
  val swiss = Nationality("Swiss")
}

case class Accountant(nationality: Option[Nationality])
object Accountant {
  val nationalityP: Prism[Accountant, Nationality] = Prism[Accountant, Nationality](_.nationality)(n => Accountant(Some(n)))
}

class PrismSpec extends FreeSpec with GeneratorDrivenPropertyChecks {

  private val nationalityGen: Gen[Nationality] = for {
    s ← arbitrary[String]
  } yield Nationality(s)

  private val maybeNationalityGen: Gen[Option[Nationality]] = Gen.option(nationalityGen)

  private val accountantGen: Gen[Accountant] = for {
    maybeNationality ← maybeNationalityGen
  } yield Accountant(maybeNationality)

  "Prism" - {
    "basics" - {
      "by example" in {
        Accountant.nationalityP.getOption(Accountant(None)) shouldBe None
        Accountant.nationalityP.getOption(Accountant(Some(british))) shouldBe Some(british)

        Accountant.nationalityP.reverseGet(british) shouldBe Accountant(Some(british))
      }

      "property based" in {
        forAll(maybeNationalityGen){(maybeNationality) =>
          Accountant.nationalityP.getOption(Accountant(maybeNationality)) shouldBe maybeNationality
        }

        forAll(maybeNationalityGen){(maybeNationality) =>
          Accountant.nationalityP.getOption(Accountant(maybeNationality)) shouldBe maybeNationality
        }
      }
    }

    "properties" - {
      "by example" in {
        val britishAccountant = Accountant(Some(british))
        Accountant.nationalityP.getOption(britishAccountant).map(Accountant.nationalityP.reverseGet) shouldBe Some(britishAccountant)
        Accountant.nationalityP.getOption(Accountant(None)).map(Accountant.nationalityP.reverseGet) shouldBe None

        Accountant.nationalityP.getOption(Accountant.nationalityP.reverseGet(british)) shouldBe Some(british)
      }

      "property based" in {
        forAll(accountantGen){accountant =>
          val left: Option[Accountant] = Accountant.nationalityP.getOption(accountant).map(Accountant.nationalityP.reverseGet)
          whenever(left.isDefined) {left shouldBe Some(accountant)}
        }

        forAll(nationalityGen){ nationality =>
          Accountant.nationalityP.getOption(Accountant.nationalityP.reverseGet(nationality)) shouldBe Some(nationality)
        }
      }
    }

    "modify" in {
      val makeAccountantsNationalityGreatAgain: Accountant => Accountant = Accountant.nationalityP.modify(n => n.copy(text = n.text.toUpperCase))

      makeAccountantsNationalityGreatAgain(Accountant(Some(Nationality("Spanish")))) shouldBe Accountant(Some(Nationality("SPANISH")))
      makeAccountantsNationalityGreatAgain(Accountant(None)) shouldBe Accountant(None)
    }
  }
}
