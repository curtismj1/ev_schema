package com.fuego.contextvalidation

import play.api.libs.json
import play.api.libs.json._

import scala.util.Try

trait Parser[T] {
  def parse(str: String): T
}

object JsonHelpers {
  implicit class toJson(str: String) {
    def toVal: Either[ValidationReport, JsValue] =
      Try(Json.parse(str)).toEither.left.map(
        exception =>
          ValidationReport.failed(
            s"Exception occurred when trying to parse $str to json ${exception.getMessage}"
          )
      )
  }
  implicit class RuleConverter[A, B <: ValidationReport](func: A => B) {
    def toRule: TestRule[A, B] = (a: A) => func(a)
  }

}

/**
  * In order to write a [[RuleDocumentParser()]], we will be recursively traversing a JSON document,
  * and building a list of functions based on a [[Map]] of keywords have to be able to describe the following things:
  *   1. What keywords activate a given rule?
  *   2. What functions do those keywords produce?
  *   3. What happens if we iterate to the full depth of a document without finding any keywords?
  */
 case class RuleDocumentParser(
    keywordMap: Map[String, JsValue => TestRule[String, ValidationReport]] =
      RuleDocumentParser.defaultKeywordMap,
    ruleReducer: TestRuleReducer[JsValue, ValidationReport] =
      RuleReducers.and[JsValue],
    currentPathExtractor: JsPath = JsPath
) extends Parser[TestRule[String, ValidationReport]] {

  def parse(str: String): TestRule[String, ValidationReport] = {
    import JsonHelpers._
    val jsVal   = Json.parse(str)
    val valRule = parseVal(jsVal)
    inputStr => {
      val either =
        for {
          asVal <- inputStr.toVal
        } yield valRule.validate(asVal)
      either.merge
    }
  }

  def withRule(keyword: String, func: JsValue => TestRule[String, ValidationReport]): RuleDocumentParser = {
    copy(keywordMap = keywordMap + (keyword -> func))
  }

  def parseStr(jsString: JsString): TestRule[JsValue, ValidationReport] = {
      if (jsString.value.equals("*"))
        return _ => ValidationReport.passed("Value at path exists")
      val strRule =
      jsString.value match {
        case s if s.startsWith("/")
          && s.endsWith("/")
          && s.length > 2 =>
              RegexValidationReportSupplier(s.substring(1, s.length-1))
        case s => StringEqualsTestRule(s)
      }
    (jsVal) => {
      jsVal match {
        case jsString: JsString => strRule(jsString.value)
        case _ =>
          ValidationReport.failed(s"Expected value to be string but was ${jsVal.getClass}")
      }
    }
  }

  def parseVal(jsVal: JsValue): TestRule[JsValue, ValidationReport] =
    jsVal match {
      case jsObj: JsObject => parseObj(jsObj)
      case jsArr: JsArray  => parseArr(jsArr)
      case jsString: JsString => parseStr(jsString)
      case _ =>
        _ => ValidationReport.failed("Could not find test rule")
    }

  //Should this iterate through all the key values to extract what is in the map and then recurse down?
  //How should it handle all the paths?
  def parseObj(jsObj: JsObject): TestRule[JsValue, ValidationReport] = {
    //collection.Set[TestRule[JsValue, ValidationReport]]
    val rules =
      for {
        (key, value) <- jsObj.fieldSet
        keywordFunc = keywordMap
          .get(key)
          .map(_(value))
          .map(func => {
            val jsFunc: JsValue => ValidationReport = {
              case s: JsString => func(s.value)
              case a =>
                ValidationReport.failed(s"Value ${a} is not of type String")
            }
            import JsonHelpers.RuleConverter
            jsFunc.toRule
          })
          .orElse( key match {
            case "AND" => Some(copy(ruleReducer = RuleReducers.and).parseVal(value))
            case "OR" => Some(copy(ruleReducer = RuleReducers.or).parseVal(value))
            case _ => None
          })
          .getOrElse(
              if (key.endsWith("?"))
                JsonPathRule.optional(JsPath \ key.dropRight(1), parseVal(value))
              else JsonPathRule.required(JsPath \ key, parseVal(value))
          )
      } yield keywordFunc
    if (rules.size == 1) rules.head else ruleReducer(rules.toList)
  }

  def parseArr(jsArr: JsArray): TestRule[JsValue, ValidationReport] =
    ruleReducer(jsArr.value.map(parseVal).toList)
}

case class RuleDocumentParserBuilder(
    keywordMap: Map[String, JsValue => String => ValidationReport]
) {
  def withBaseKeywordMap(
      keywordMap: Map[String, JsValue => String => ValidationReport]
  ): RuleDocumentParserBuilder = {
    this.copy(keywordMap)
  }
}

object RuleDocumentParser {

  def getDefault: RuleDocumentParser = RuleDocumentParser()

  // TODO: Come back to this and think about how to avoid the casting issues
  val defaultKeywordMap
      : Map[String, JsValue => TestRule[String, ValidationReport]] = Map(
    "equals" -> (
        (jsVal: JsValue) =>
          StringEqualsTestRule(jsVal.asInstanceOf[JsString].value)
      ),
    "regex" -> (
        (jsVal: JsValue) =>
          RegexValidationReportSupplier(jsVal.asInstanceOf[JsString].value)
      ),
    "contains" -> (
        (jsVal: JsValue) =>
          StringContainsTestRule(
            jsVal
              .asInstanceOf[json.JsArray]
              .value
              .toVector
              .map(_.asInstanceOf[JsString].value)
          )
      ),
    "orContains" -> (
        (jsVal: JsValue) =>
          StringContainsTestRule(
            jsVal
              .asInstanceOf[json.JsArray]
              .value
              .toVector
              .map(_.asInstanceOf[JsString].value)
          )
      )
  )
}
