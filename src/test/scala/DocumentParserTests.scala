import org.scalatest.{FlatSpec, Matchers}

class DocumentParserTests extends FlatSpec with Matchers {

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

    val validPayload =
      """
        |{
        |   "v1": "test"
        |}
      """.stripMargin

  }

}
