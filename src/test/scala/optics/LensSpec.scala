package optics

import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary

case class EnglishSpanish(englishTerm: String, spanishTerm: String)
object EnglishSpanish {
  val englishTermL: Lens[EnglishSpanish, String] =
    Lens[EnglishSpanish, String](_.englishTerm){case (et, es) => es.copy(englishTerm = et)}

  val asTupleI: Iso[EnglishSpanish, (String, String)] =
    Iso[EnglishSpanish, (String, String)](engEsp => (engEsp.englishTerm, engEsp.spanishTerm)) { case (eng, esp) =>
      EnglishSpanish(eng, esp)
    }
}

class LensSpec extends FreeSpec with GeneratorDrivenPropertyChecks {
  def firstL[A, B]: Lens[(A, B), A] = Lens[(A, B), A](_._1) { case (s, (_, s2)) => (s, s2) }
  def secondL[A, B]: Lens[(A, B), B] = Lens[(A, B), B](_._2) { case (s, (s1, _)) => (s1, s) }

  private def lensSatisfiesProperties[S, A](lens: Lens[S, A])(sGen: Gen[S], aGen: Gen[A]): Unit = {
    forAll(sGen) { s =>
      lens.set(lens.get(s), s) shouldBe s
    }
    forAll(sGen, aGen) { (s, a) =>
      lens.get(lens.set(a, s)) shouldBe a
    }
  }

  "Lens" - {
    "basics" - {
      "by example (EnglishSpanish)" in {
        val es = EnglishSpanish("hi", "ohtuporaqui")
        EnglishSpanish.englishTermL.get(es) shouldBe "hi"
        EnglishSpanish.englishTermL.set("youagain?", es) shouldBe EnglishSpanish("youagain?", "ohtuporaqui")
      }

      "by example (tuples)" in {
        val tuple = ("foo", 1)
        firstL.get(tuple) shouldBe "foo"
        firstL.set("bar", tuple) shouldBe ("bar", 1)
      }

      "property based" in {
        forAll{ (s: String, i: Int) =>
          firstL.get((s,  i)) shouldBe s
        }
        forAll{ (s1: String, s2: String, i: Int) =>
          firstL.set(s2, (s1,  i)) shouldBe (s2, i)
        }
      }
    }

    "properties" - {
      "by example (EnglishSpanish)" in {
        val es = EnglishSpanish("hi", "ohtuporaqui")
        EnglishSpanish.englishTermL.set(EnglishSpanish.englishTermL.get(es), es) shouldBe es
        EnglishSpanish.englishTermL.get(EnglishSpanish.englishTermL.set("youagain?", es)) shouldBe "youagain?"
      }

      "by example (tuple)" in {
        val tuple = ("foo", 1)
        firstL.set(firstL.get(tuple), tuple) shouldBe tuple
        firstL.get(firstL.set("bar", tuple)) shouldBe "bar"
      }

      "property based" in {
        forAll { (tuple: (String, Int)) =>
          firstL.set(firstL.get(tuple), tuple) shouldBe tuple
        }
        forAll { (tuple: (String, Int), s: String) =>
          firstL.get(firstL.set(s, tuple)) shouldBe s
        }
      }
    }

    "modify" in {
      firstL[String, String].modify(_.toUpperCase)(("foo", "")) shouldBe ("FOO", "")
    }

    "lens1 compose lens2" in {
      val fstSnd: Lens[((String, Int), String), Int] = firstL[(String, Int), String] compose secondL[String, Int]

      val dup = fstSnd.modify(_ * 2)

      dup((("foo", 10), "bar")) shouldBe(("foo", 20), "bar")

      lensSatisfiesProperties(fstSnd)(arbitrary[((String, Int), String)], arbitrary[Int])
    }
  }
}
