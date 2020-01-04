import com.fuego.validation.RuleDocumentParser
import org.scalatest.{FlatSpec, Matchers}

class RuleDocumentParserTests extends FlatSpec with Matchers {

  val ruleDocumentParser = new RuleDocumentParser()
  "equals keyword" should "be valid according to equality" in {
    val doc =
      """{
        |   "equals": "foo"
        |}""".stripMargin
    val ruleSupplier = ruleDocumentParser.parse(doc)
    assert(ruleSupplier("foo").passed(), "Rule expecting 'foo' should pass when given 'foo'")
    assert(ruleSupplier("bar").failed(), "Rule expecting 'foo' should fail when given 'bar'")
  }

  "regex keyword" should "pass for matching regex, fail for non-matching regex" in {
    val doc =
      """{
        |   "regex": "foo"
        |}""".stripMargin
    val ruleSupplier = ruleDocumentParser.parse(doc)
    assert(ruleSupplier("some_foo").passed(), "Regex rule expecting 'foo' should pass when given 'some_foo'")
    assert(ruleSupplier("bar").failed(), "Rule expecting 'foo' should fail when given 'bar'")
  }

  "contains keyword" should "pass when all target substrings are in the given string, fail if not" in {
    val doc =
      """{
        |   "contains": ["foo", "bar"]
        |}""".stripMargin
    val ruleSupplier = ruleDocumentParser.parse(doc)
    assert(ruleSupplier("foo_bar").passed(), "Contains rule expecting 'foo' and 'bar' should pass when given 'foo_bar'")
    assert(ruleSupplier("bar_baz").failed(), "Contains rule expecting 'foo' and 'bar' should fail when given 'bar_baz'")
  }

  "orContains keyword" should "pass when at least one target substring is in the given string, fail if none are" in {
    val doc =
      """{
        |   "orContains": ["foo", "bar"]
        |}""".stripMargin
    val ruleSupplier = ruleDocumentParser.parse(doc)
    assert(ruleSupplier("foo_bar").passed(), "orContains rule expecting 'foo' or 'bar' should pass when given 'foo'")
    assert(ruleSupplier("baz").failed(), "orContains rule expecting 'foo' or 'bar' should fail when given 'baz'")
  }

}
