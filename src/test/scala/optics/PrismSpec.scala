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
  val nationalityI: Iso[Accountant, Option[Nationality]] = Iso[Accountant, Option[Nationality]](_.nationality)(Accountant.apply)
  val nationalityP: Prism[Accountant, Nationality] = Prism[Accountant, Nationality](_.nationality)(n => Accountant(Some(n)))
}

case class Company(accountant: Option[Accountant])
object Company {
  val accountantP: Prism[Company, Accountant] = Prism[Company, Accountant](_.accountant)(a => Company(Some(a)))
}

case class BusinessAccount(company: Company)
object BusinessAccount {
  val companyI: Iso[BusinessAccount, Company] = Iso[BusinessAccount, Company](_.company)(BusinessAccount.apply)
}

class PrismSpec extends FreeSpec with GeneratorDrivenPropertyChecks {

  private val nationalityGen: Gen[Nationality] = for {
    s ← arbitrary[String]
  } yield Nationality(s)

  private val maybeNationalityGen: Gen[Option[Nationality]] = Gen.option(nationalityGen)

  private val accountantGen: Gen[Accountant] = for {
    maybeNationality ← maybeNationalityGen
  } yield Accountant(maybeNationality)

  private val companyGen: Gen[Company] = for {
    maybeAccountant ← Gen.option(accountantGen)
  } yield Company(maybeAccountant)

  private val businessAccountGen: Gen[BusinessAccount] = for {
    company ← companyGen
  } yield BusinessAccount(company)

  private val makeGreatAgain: Nationality => Nationality = Nationality.textI.modify(_.toUpperCase)

  def prismSatisfiesProperties[S, A](prism: Prism[S, A])(sGen: Gen[S], aGen: Gen[A]): Unit = {
    forAll(sGen){s =>
      val left: Option[S] = prism.getOption(s).map(prism.reverseGet)
      whenever(left.isDefined) {left shouldBe Some(s)}
    }

    forAll(aGen){ a =>
      prism.getOption(prism.reverseGet(a)) shouldBe Some(a)
    }
  }

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
      val makeAccountantsNationalityGreatAgain: Accountant => Accountant = Accountant.nationalityP.modify(makeGreatAgain)

      makeAccountantsNationalityGreatAgain(Accountant(Some(Nationality("Spanish")))) shouldBe Accountant(Some(Nationality("SPANISH")))
      makeAccountantsNationalityGreatAgain(Accountant(None)) shouldBe Accountant(None)
    }

    "set" - {
      "by example" in {
        val becomeSwiss: Accountant => Accountant = Accountant.nationalityP.set(swiss)

        becomeSwiss(Accountant(Some(british))) shouldBe Accountant(Some(swiss))
        becomeSwiss(Accountant(None)) shouldBe Accountant(None)
      }

      "property based" in {
        forAll(nationalityGen, accountantGen) { (newNationality, accountant) =>
          val naturalise: Accountant => Accountant = Accountant.nationalityP.set(newNationality)

          accountant match {
            case Accountant(Some(_)) => naturalise(accountant) shouldBe Accountant(Some(newNationality))
            case Accountant(None)    => naturalise(accountant) shouldBe accountant
          }
        }
      }
    }

    "modifyOption" in {
      val maybeMakeAccountantsNationalityGreatAgain: Accountant => Option[Accountant] = Accountant.nationalityP.modifyOption(makeGreatAgain)

      maybeMakeAccountantsNationalityGreatAgain(Accountant(Some(Nationality("Spanish")))) shouldBe Some(Accountant(Some(Nationality("SPANISH"))))
      maybeMakeAccountantsNationalityGreatAgain(Accountant(None)) shouldBe None
    }

    "prism1 compose prism2" in {
      val composed: Prism[Company, Nationality] = Company.accountantP compose Accountant.nationalityP

      // modify
      val makeCompanyGA: Company => Company = composed.modify(makeGreatAgain)
      makeCompanyGA(Company(Some(Accountant(Some(Nationality("Spanish")))))) shouldBe Company(Some(Accountant(Some(Nationality("SPANISH")))))
      makeCompanyGA(Company(Some(Accountant(None)))) shouldBe Company(Some(Accountant(None)))
      makeCompanyGA(Company(None)) shouldBe Company(None)

      // modifyOption
      val maybeMakeCompanyGA: Company => Option[Company] = composed.modifyOption(makeGreatAgain)
      maybeMakeCompanyGA(Company(Some(Accountant(Some(Nationality("Spanish")))))) shouldBe Some(Company(Some(Accountant(Some(Nationality("SPANISH"))))))
      maybeMakeCompanyGA(Company(Some(Accountant(None)))) shouldBe None
      maybeMakeCompanyGA(Company(None)) shouldBe None

      prismSatisfiesProperties(composed)(companyGen, nationalityGen)
    }

    "prism compose iso" in {
      val composed: Prism[Accountant, String] = Accountant.nationalityP compose Nationality.textI

      val makeAccountantGA: Accountant => Accountant = composed.modify(_.toUpperCase)

      makeAccountantGA(Accountant(Some(Nationality("Spanish")))) shouldBe Accountant(Some(Nationality("SPANISH")))
      makeAccountantGA(Accountant(None)) shouldBe Accountant(None)

      prismSatisfiesProperties(composed)(accountantGen, arbitrary[String])
    }

    "iso compose prism" in {
      val composed: Prism[BusinessAccount, Accountant] = BusinessAccount.companyI compose Company.accountantP

      val smithAccountant = Accountant(Some(Nationality.british))
      val ackermannAccountant = Accountant(Some(Nationality.swiss))

      val changeAccountant: BusinessAccount => BusinessAccount = composed.set(ackermannAccountant)

      changeAccountant(BusinessAccount(Company(Some(smithAccountant)))) shouldBe BusinessAccount(Company(Some(ackermannAccountant)))
      changeAccountant(BusinessAccount(Company(None))) shouldBe BusinessAccount(Company(None))

      prismSatisfiesProperties(composed)(businessAccountGen, accountantGen)
    }
  }

  "asPrism.below" - {
    "Navigates from a parent into an optional attribute ones and beyond" in {
      val bo: Prism[Option[Accountant], Option[Nationality]] = Accountant.nationalityP.belowOption

      Accountant.nationalityP.getOption(Accountant(None)) shouldBe None
      Accountant.nationalityP.getOption(Accountant(Some(british))) shouldBe Some(british)

      bo.getOption(None) shouldBe Some(None)
      bo.getOption(Some(Accountant(None))) shouldBe None
      bo.getOption(Some(Accountant(Some(british)))) shouldBe Some(Some(british))

      bo.reverseGet(None) shouldBe None
      bo.reverseGet(Some(british)) shouldBe Some(Accountant(Some(british)))

      val blankNationality = Nationality("")
      val reverseNationality: Nationality => Nationality = {
        case Nationality(t) => Nationality(t.reverse)
      }
      val blankOrReverseNationality: Option[Nationality] => Option[Nationality] = _.orElse(Some(blankNationality)).map(reverseNationality)
      blankOrReverseNationality(None) shouldBe Some(blankNationality)

      bo.modifyOption(blankOrReverseNationality)(Some(Accountant(Some(Nationality("Brit"))))) shouldBe Some(Some(Accountant(Some(Nationality("tirB")))))
      bo.modifyOption(blankOrReverseNationality)(Some(Accountant(None))) shouldBe None
      bo.modifyOption(blankOrReverseNationality)(None) shouldBe Some(Some(Accountant(Some(blankNationality))))

      bo.modify(blankOrReverseNationality)(Some(Accountant(Some(Nationality("Brit"))))) shouldBe Some(Accountant(Some(Nationality("tirB"))))
      bo.modify(blankOrReverseNationality)(Some(Accountant(None))) shouldBe Some(Accountant(None))
      bo.modify(blankOrReverseNationality)(None) shouldBe Some(Accountant(Some(blankNationality)))


      Nationality.textI.get(Nationality("British")) shouldBe "British"
      Nationality.textI.reverseGet("British") shouldBe Nationality("British")

      // iso.asPrism.getOption(...) should always return Some(...) -- TODO make some laaaaawws
      Nationality.textI.asPrism.getOption(Nationality("British")) shouldBe Some("British")
      Nationality.textI.asPrism.reverseGet("British") shouldBe Nationality("British")
      Nationality.textI.asPrism.belowOption.getOption(Some(Nationality("British"))) shouldBe Some(Some("British"))
      Nationality.textI.asPrism.belowOption.getOption(None) shouldBe Some(None)
      Nationality.textI.asPrism.belowOption.reverseGet(Some("British")) shouldBe Some(Nationality("British"))
      Nationality.textI.asPrism.belowOption.reverseGet(None) shouldBe None

      val chained: Prism[Accountant, Option[String]] = Accountant.nationalityI compose Nationality.textI.asPrism.belowOption
      chained.set(Some("Homeless refugee"))(Accountant(Some(Nationality("British")))) shouldBe Accountant(Some(Nationality("Homeless refugee")))
      chained.set(Some("Homeless refugee"))(Accountant(None)) shouldBe Accountant(Some(Nationality("Homeless refugee")))
    }
  }
}
