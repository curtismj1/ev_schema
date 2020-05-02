import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fuego.validation.{AndReport, OrReport, RuleDocumentParser, TestRule, ValidationReport}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

class DocumentParserTests extends FlatSpec with Matchers {

  val ruleDocParser = RuleDocumentParser()

  "AND keyword" should "return an com.fuego.validation.AndTestRule" in {
    val andDoc =
      """
        |{
        |   "AND": {
        |     "v1": {
        |        "equals": "test"
        |      }
        |   }
        |}
      """.stripMargin
    val validDoc =
      """{
        |  "v1": "test"
        |}""".stripMargin
    val invalidDoc =
    """{
       |  "v3": "test"
       |}""".stripMargin
    val rule: TestRule[String, ValidationReport] = ruleDocParser.parse(andDoc)
    val validReport = rule.validate(validDoc)
    val invalidReport = rule.validate(invalidDoc)
    assert(validReport.passed(), "A valid payload should pass " + validReport.description)
    assert(invalidReport.failed(), "An invalid payload should fail " + invalidReport.description)
  }

  "Nested Doc" should "work" in {
    val doc =
      """
      |{
      |  "v1": {
      |     "v2": {
      |        "equals": "test"
      |     },
      |     "v3": {
      |       "v4": {
      |         "equals": "test"
      |       }
      |     }
      |  }
      |}
      """.stripMargin
    val validDoc =
      """{
        |  "v1": {
        |      "v2": "test",
        |      "v3": { "v4": "test" }
        |   }
        |}""".stripMargin
    val report = RuleDocumentParser().parse(doc).validate(validDoc)
    print(Json.prettyPrint(Json.toJson(report.serialize)))
  }

  "OR keyword" should "return an com.fuego.validation.OrTestRule" in {
    val orDoc =
      """
        |{
        |   "OR": {
        |     "v1": {
        |        "equals": "test"
        |      },
        |      "v2": {
        |         "equals": "test"
        |      }
        |   }
        |}
      """.stripMargin
    val validDoc =
      """{
        |  "v1": "test",
        |  "v2": "not test"
        |}""".stripMargin
    val invalidDoc =
      """{
        |  "v2": "not test"
        |}""".stripMargin
    val rule: TestRule[String, ValidationReport] = ruleDocParser.parse(orDoc)
    val validReport = rule.validate(validDoc)
    val invalidReport = rule.validate(invalidDoc)
    print(Json.prettyPrint(Json.toJson(validReport.serialize)))
    print(Json.prettyPrint(Json.toJson(invalidReport.serialize)))
    assert(validReport.passed(), "A valid payload should pass " + validReport.description)
    assert(invalidReport.failed(), "An invalid payload should fail " + invalidReport.description)
  }

}
