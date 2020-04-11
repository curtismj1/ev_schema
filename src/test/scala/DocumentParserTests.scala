import com.fuego.validation.{RuleDocumentParser}
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
    val testParser = ruleDocParser.parse(andDoc)
    val validResult = ruleDocParser.parse(andDoc)(validDoc)
    print(ruleDocParser.parse(andDoc)(validDoc)
      .description)
  }



}
