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

class IsoSpec extends FreeSpec {
  "Iso" - {
    "basic behaviour" in {
      FamilyName.textI.get(FamilyName("Smith")) shouldBe "Smith"
      FamilyName.textI.reverseGet("Smith") shouldBe FamilyName("Smith")
    }
  }
}
