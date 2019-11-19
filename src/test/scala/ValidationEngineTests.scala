import java.util.Arrays

import org.scalatest._
import play.api.libs.json.{JsObject, JsPath, JsString, Json}

class ValidationEngineTests extends FlatSpec with Matchers {

  "root context test rule" should "be true" in {
    val testRule = ContextTestRule[TestRule](() => true)
    //ValidationEngine.validate(Arrays.asList(testRule))
  }

  "root context" should "be true" in {
    val testRule =
      ValidationEngine.validate(
        Arrays.asList(ContextTestRule[TestRule](() => true)))
  }

  "Json path test rule extractor" should "be true when path is valid, false if not" in {
    val jsonObjTestRuleSupplier =
      JsonObjTestRuleSupplier(JsPath \ "test",
                              _ => TestRule.passed("key test exists"))
    val validObj = JsObject(Seq("test" -> JsString("exists")))
    val invalidObj = JsObject(Seq("no_test" -> JsString("doesn't exist")))
    assert(jsonObjTestRuleSupplier(validObj).passed(),
           "an object with a valid path should be true")
    assert(jsonObjTestRuleSupplier(invalidObj).failed(),
           "an object without a valid path should be false")
  }

  "Test" should "" in {
    val test: Map[String, Vector[String]] =
      Map.empty.withDefaultValue(Vector.empty)
    test("test")
    val engine = ValidationEngine(ruleForPath = Map(JsPath \ "v1" -> (value =>
      StringEqualsTestRule("test", value.asInstanceOf[JsString].value))))
    val validJson =
      """
        |{
        |   "v1": "test"
        |}
      """.stripMargin
    val results = engine.validate(Json.parse(validJson))
    print(results.description)
    assert(results.passed())
  }

}
