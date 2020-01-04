import com.fuego.validation.DocumentParser
import org.scalatest.{FlatSpec, Matchers}

class DocumentParserTests extends FlatSpec with Matchers {

  val docParser = new DocumentParser()


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
  }



}
