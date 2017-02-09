package optics

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

case class FamilyName(text: String)
object FamilyName {
  val textI: Iso[FamilyName, String] = new Iso[FamilyName, String]{
    override def get: (FamilyName) => String = _.text
    override def reverseGet: (String) => FamilyName = FamilyName.apply
  }
}

case class Entry(name: String, value: String)
object Entry {
  val asTupleI: Iso[Entry, (String, String)] = new Iso[Entry, (String, String)] {
    override def get: (Entry) => (String, String) = e => (e.name, e.value)
    override def reverseGet: ((String, String)) => Entry = {case (n, v) => Entry(n, v)}
  }
}

class IsoSpec extends FreeSpec {
  "Iso" - {
    "basic behaviour" in {
      FamilyName.textI.get(FamilyName("Smith")) shouldBe "Smith"
      FamilyName.textI.reverseGet("Smith") shouldBe FamilyName("Smith")
    }

    "tuple example" in {
      Entry.asTupleI.get(Entry("id", "0021")) shouldBe ("id", "0021")
      Entry.asTupleI.reverseGet(("id", "0021")) shouldBe Entry("id", "0021")
    }

    "basic properties" in {
      FamilyName.textI.reverseGet(FamilyName.textI.get(FamilyName("Smith"))) shouldBe FamilyName("Smith")
      FamilyName.textI.get(FamilyName.textI.reverseGet("Smith")) shouldBe "Smith"
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
  }
}
