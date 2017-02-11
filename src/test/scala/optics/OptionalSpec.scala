package optics

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

case class EnterpriseAgileId(text: String)

case class Story(title: String, id: Option[EnterpriseAgileId])
object Story {
  val idO: Optional[Story, EnterpriseAgileId] = Optional[Story, EnterpriseAgileId](_.id){case (id, s) => s.copy(id = s.id.map(_ => id))}
}

class OptionalSpec extends FreeSpec with GeneratorDrivenPropertyChecks {
  private val enterpriseAgileIdGen: Gen[EnterpriseAgileId] = for {
    s ← arbitrary[String]
  } yield EnterpriseAgileId(s)

  private val maybeEnterpriseAgileIdGen: Gen[Option[EnterpriseAgileId]] = for {
    maybeString ← arbitrary[Option[String]]
  } yield maybeString.map(EnterpriseAgileId.apply)

  private val storyGen: Gen[Story] = for {
    title    ← arbitrary[String]
    maybeEAI ← maybeEnterpriseAgileIdGen
  } yield Story(title, maybeEAI)

  "optional" - {
    val eai1 = EnterpriseAgileId("jira-d1")
    val eai2 = EnterpriseAgileId("jira-d2")

    "basics" - {
      "by example" in {
        Story.idO.getOption(Story("Do the thing", Some(eai1))) shouldBe Some(eai1)
        Story.idO.getOption(Story("Do the thing", None)) shouldBe None

        Story.idO.set(eai2, Story("Do the thing", Some(eai1))) shouldBe Story("Do the thing", Some(eai2))
      }

      "property based" in {
        forAll(arbitrary[String], enterpriseAgileIdGen){(title, eai) =>
          Story.idO.getOption(Story(title, Some(eai))) shouldBe Some(eai)
        }
        forAll{(title: String) =>
          Story.idO.getOption(Story(title, None)) shouldBe None
        }

        forAll(arbitrary[String], enterpriseAgileIdGen, enterpriseAgileIdGen){(title, eai1, eai2) =>
          Story.idO.set(eai2, Story(title, Some(eai1))) shouldBe Story(title, Some(eai2))
        }
        forAll(arbitrary[String], enterpriseAgileIdGen){(title, eai) =>
          val story = Story(title, None)
          Story.idO.set(eai, story) shouldBe story
        }
      }
    }

    "properties" - {
      "by example" in {
        val s = Story("Do the thing", Some(eai1))
        Story.idO.getOption(s).map(Story.idO.set(_, s)) shouldBe Some(s)
        Story.idO.getOption(Story("Refactor", None)).map(Story.idO.set(_, s)) shouldBe None

        Story.idO.getOption(Story.idO.set(eai1, s)) shouldBe Some(eai1)
        Story.idO.getOption(Story.idO.set(eai1, Story("Refactor", None))) shouldBe None
      }

      "property based" in {
        forAll(storyGen){story =>
          val maybeStory = Story.idO.getOption(story).map(Story.idO.set(_, story))
          // Note: according http://www.slideshare.net/JulienTruffaut/beyond-scala-lens it should be just
          // maybeStory shouldBe (story). However, I think we can also get a None when the original story's id is None
          whenever(maybeStory.isDefined){maybeStory shouldBe Some(story)}
        }
        forAll(storyGen, enterpriseAgileIdGen){(story, eai) =>
          val maybeEai: Option[EnterpriseAgileId] = Story.idO.getOption(Story.idO.set(eai, story))
          whenever(maybeEai.isDefined){maybeEai shouldBe Some(eai)}
        }
      }
    }

    "modifyOption" in {
      val changeId = Story.idO.modifyOption(_ => eai2)

      changeId(Story("Do the thing", Some(eai1))) shouldBe Some(Story("Do the thing", Some(eai2)))
      changeId(Story("Refactor", None)) shouldBe None
    }
  }
}
