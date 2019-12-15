import org.scalatest.{FlatSpec, Matchers}

class RuleDocumentParserTests extends FlatSpec with Matchers {

  val ruleDocumentParser = new RuleDocumentParser()
  "equals keyword" should "be valid according to equality" in {
    val equalDoc =
      """{
        |   "equals": "foo"
        |}""".stripMargin
    val ruleSupplier = ruleDocumentParser.parse(equalDoc)
    assert(ruleSupplier("foo").passed(), "Rule expecting 'foo' should pass when given 'foo'")
    assert(ruleSupplier("bar").failed(), "Rule expecting 'foo' should fail when given 'bar'")
  }

}
