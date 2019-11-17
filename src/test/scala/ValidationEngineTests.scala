import java.util.Arrays

import org.scalatest._

class ValidationEngineTests extends FlatSpec with Matchers {

  "root context test rule" should "be true" in {
    val testRule = ContextTestRule(TestRule.PASSED)
    ValidationEngine.validate(Arrays.asList(testRule))
  }

  "root context" should "be true" in {
    val testRule =
      ValidationEngine.validate(Arrays.asList(ContextTestRule(TestRule.PASSED)))
    print(testRule.description)
  }

  "try" should "be cool" in {

  }

}
