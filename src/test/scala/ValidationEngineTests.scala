import org.scalatest._
import play.api.libs.json.{JsObject, JsPath, JsString, JsValue, Json}

class ValidationEngineTests extends FlatSpec with Matchers {

  "Json path test rule extractor" should "be true when path is valid, false if not" in {
    def ruleFunc(rule: JsValue): TestRule = StringEqualsTestRule("test", rule.asInstanceOf[JsString].value)
    val engine = ValidationEngine().withRuleFunc(ruleFunc)
      .forPath(JsPath \ "v1")
      .add
    val validJson =
      """
        | {
        |   "v1": "test"
        | }
      """.stripMargin
    val invalidJson =
      """
        | {
        |   "non_existent_path": "test"
        | }
      """.stripMargin
    assert(engine.validate(Json.parse(validJson)).passed(),
           "an object with a valid path should be true")
    assert(engine.validate(Json.parse(invalidJson)).failed(),
           "an object without a valid path should be false")
  }

  "Valid json" should "Return valid test rule" in {
    val test: Map[String, Vector[String]] =
      Map.empty.withDefaultValue(Vector.empty)
    test("test")
    val engine = ValidationEngine().withRuleFunc(TestRule.and(
      value => StringEqualsTestRule("test", value.asInstanceOf[JsString].value),
      value => RegexTestRule("es", value.asInstanceOf[JsString].value)
    )).forPath(JsPath \ "v1").add
    val validJson =
      """
        |{
        |   "v1": "test"
        |}
      """.stripMargin
    val results = engine.validate(Json.parse(validJson))
    assert(results.passed())
  }

  "Failing rule whose required context is not activated by any rules" should "Pass" in {
    val inputVal = JsString("test")
    val testResults = ValidationEngine()
      .withRuleFunc(_ => TestRule.passed("root passed"))
      .forRootPath().add
      .withRuleFunc(_ => TestRule.failed("nonactive_context rule failed"))
      .forRootPath().withRequiresContext("nonactive_context").add
      .validate(inputVal)
    assert(testResults.passed())
  }

  "Failing rule whose required context is activated by any rules" should "Pass" in {
    val inputVal = JsString("test")
    val testResults = ValidationEngine()
      .withRuleFunc(_ => TestRule.passed("root passed"))
      .forRootPath().withActivatesContext("active_context").add
      .withRuleFunc(_ => TestRule.failed("nonactive_context rule failed"))
      .forRootPath().withRequiresContext("active_context").add
      .validate(inputVal)
    assert(testResults.failed())
  }

}
