import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fuego.contextvalidation.{AndReport, OrReport, RuleDocumentParser, TestRule, ValidationReport}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

class DocumentParserTests extends FlatSpec with Matchers {

  val ruleDocParser = RuleDocumentParser()

  "AND keyword" should "return an AndTestRule" in {
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

  "Nested Doc with optional" should "pass when object is absent, fail when present and not correct" in {
    val doc =
      """
      |{
      |  "v1": {
      |     "v2": {
      |        "equals": "test"
      |     },
      |     "v3": {
      |       "v4?": {
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
    val validDocNoOptional =
      """{
        |  "v1": {
        |      "v2": "test",
        |      "v3": { "v5": "test" }
        |   }
        |}""".stripMargin
    val invalidDoc =
      """{
        |  "v1": {
        |      "v2": "test",
        |      "v3": { "v4": "not test" }
        |   }
        |}""".stripMargin
    val validReport = RuleDocumentParser().parse(doc).validate(validDoc)
    val validReportNoOptional = RuleDocumentParser().parse(doc).validate(validDocNoOptional)
    val invalidReportNoOptional = RuleDocumentParser().parse(doc).validate(invalidDoc)
    print(Json.prettyPrint(Json.toJson(validReport.serialize)))
    print(Json.prettyPrint(Json.toJson(validReportNoOptional.serialize)))
    print(Json.prettyPrint(Json.toJson(invalidReportNoOptional.serialize)))
    assert(validReport.passed(), "Valid report with all fields ")
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

  "rule matching /.*somePattern/" should "validate with regex test rule" in {
    val ruleDoc =
      """{
        |  "v1": {
        |      "v2": "/.*somePattern/"
        |   }
        |}""".stripMargin
    val validDoc =
      """{
        |  "v1": {
        |      "v2": "SomePrefix to somePattern"
        |   }
        |}""".stripMargin
    val invalidDoc =
      """{
        |  "v1": {
        |      "v2": "SomePrefix to wrongPattern"
        |   }
        |}""".stripMargin
    val rule: TestRule[String, ValidationReport] = ruleDocParser.parse(ruleDoc)
    val validReport = rule.validate(validDoc)
    val invalidReport = rule.validate(invalidDoc)
    print(Json.prettyPrint(Json.toJson(validReport.serialize)))
    print(Json.prettyPrint(Json.toJson(invalidReport.serialize)))
    assert(validReport.passed(), "A valid payload should pass " + validReport.description)
    assert(invalidReport.failed(), "An invalid payload should fail " + invalidReport.description)
  }

  "Equals rule" should "be default for terminating jsStrng" in {
    val ruleDoc =
      """{
        |  "v1": {
        |      "v2": "someValue"
        |   }
        |}""".stripMargin
    val validDoc =
      """{
        |  "v1": {
        |      "v2": "someValue"
        |   }
        |}""".stripMargin
    val invalidDoc =
      """{
        |  "v1": {
        |      "v2": "not someValue"
        |   }
        |}""".stripMargin
    val rule: TestRule[String, ValidationReport] = ruleDocParser.parse(ruleDoc)
    val validReport = rule.validate(validDoc)
    val invalidReport = rule.validate(invalidDoc)
    print(Json.prettyPrint(Json.toJson(validReport.serialize)))
    print(Json.prettyPrint(Json.toJson(invalidReport.serialize)))
    assert(validReport.passed(), "A valid payload should pass " + validReport.description)
    assert(invalidReport.failed(), "An invalid payload should fail " + invalidReport.description)
  }

}
