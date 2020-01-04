import com.fuego.validation.{RuleDocumentParser, TestRule}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.{JsObject, JsString}

class CombinatorDocumentParserTests extends FlatSpec with Matchers {
  val ruleParser = RuleDocumentParser()
  "AND keyword" should "generate AND test rule" in {
    val andDoc =
      """
        |{
        |   "AND": {
        |      "equals": "test",
        |      "regex" : ".*es"
        |   }
        |}
        |""".stripMargin
    val rules: String => TestRule = ruleParser.parse(andDoc)
    val validTestRule             = rules("test")
    val invalidTestRule           = rules("es")
    assert(
      validTestRule.passed(),
      "rule which passes for all tests when combined with AND logic should pass")
    assert(
      invalidTestRule.failed(),
      "rule which passes for only one rule when combined with AND logic should fail")
  }

  "OR keyword" should "generate OR test rule" in {
    val orDoc =
      """
        |{
        |   "OR": {
        |      "equals": "test",
        |      "regex" : ".*es"
        |   }
        |}
        |""".stripMargin
    val rules: String => TestRule = ruleParser.parse(orDoc)
    val validTestRule             = rules("es")
    val invalidTestRule           = rules("no")
    assert(
      validTestRule.passed(),
      "rule which passes for one tests when combined with OR logic should pass")
    assert(
      invalidTestRule.failed(),
      "rule which passes for only one rule when combined with OR logic should fail")
  }
}
