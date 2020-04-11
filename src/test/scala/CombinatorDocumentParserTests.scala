import com.fuego.validation.{RuleDocumentParser, ValidationReport}
import org.scalatest.{FlatSpec, Matchers}

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
    val rules: String => ValidationReport = ruleParser.parse(andDoc)
    val validValidationReport             = rules("test")
    val invalidValidationReport           = rules("es")
    assert(
      validValidationReport.passed(),
      "rule which passes for all tests when combined with AND logic should pass")
    assert(
      invalidValidationReport.failed(),
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
    val rules: String => ValidationReport = ruleParser.parse(orDoc)
    val validValidationReport             = rules("es")
    val invalidValidationReport           = rules("no")
    assert(
      validValidationReport.passed(),
      "rule which passes for one tests when combined with OR logic should pass")
    assert(
      invalidValidationReport.failed(),
      "rule which passes for only one rule when combined with OR logic should fail")
  }
}
