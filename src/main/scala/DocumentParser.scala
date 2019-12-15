import java.util

import play.api.libs.json.{JsArray, JsObject, JsPath, JsString, JsValue, Json}
import java.util.{List => JList}

import play.api.libs.json

import scala.jdk.CollectionConverters._

trait Parser[T] {
  def parse(str: String): T
}

/**
  * In order to write a [[RuleDocumentParser()]], we will be recursively traversing a JSON document,
  * and building a list of functions based on a [[Map]] of keywords have to be able to describe the following things:
  *   1. What keywords activate a given rule?
  *   2. What functions do those keywords produce?
  *   3. What happens if we iterate to the full depth of a document without finding any keywords?
  */
case class RuleDocumentParser(
    keywordMap: Map[String, JsValue => String => TestRule] =
      RuleDocumentParser.defaultKeywordMap
) extends Parser[String => TestRule] {
  def parse(rulesDoc: String): String => TestRule = {
    val jsVal = Json.parse(rulesDoc)
    parse(jsVal)
  }

  def parse(jsVal: JsValue): String => TestRule =
    jsVal match {
      case jsObj: JsObject => parse(jsObj)
      case jsArr: JsArray  => parse(jsArr)
      case _ =>
        _ => TestRule.failed("Could not find test rule")
    }

  def parse(jsObj: JsObject): String => TestRule = {
    jsObj
    // TODO: Implement this
    _ => TestRule.failed("")
  }
  def parse(jsArr: JsArray): String => TestRule = { _ =>
    TestRule.failed("")
  }

}

object RuleDocumentParser {
  // TODO: Come back to this and think about how to avoid the casting issues
  val defaultKeywordMap: Map[String, JsValue => (String => TestRule)] = Map(
    "equals" -> (
        (jsVal: JsValue) =>
          StringEqualsTestRuleSupplier(jsVal.asInstanceOf[JsString].value)
      ),
    "regex" -> (
        (jsVal: JsValue) =>
          RegexTestRuleSupplier(jsVal.asInstanceOf[JsString].value)
      ),
    "contains" -> (
        (jsVal: JsValue) =>
          StringContainsTestRuleSupplier(
            jsVal
              .asInstanceOf[json.JsArray]
              .value
              .toArray
              .map(_.asInstanceOf[JsString].value)
              .toBuffer
              .asJava
          )
      ),
    "orContains" -> (
        (jsVal: JsValue) =>
          StringContainsTestRuleSupplier(
            jsVal
              .asInstanceOf[json.JsArray]
              .value
              .toArray
              .map(_.asInstanceOf[JsString].value)
              .toBuffer
              .asJava
          )
      )
  )
}
class DocumentParser {

  def traverseVal(
      jsVal: JsValue,
      currentContext: Set[String],
      parseAsRules: Boolean
  ): ValidationEngine = {
    jsVal match {
      case obj: JsObject => traverseObj(obj, currentContext, parseAsRules)
      case arr: JsArray  => traverseArr(arr)
      case _             => ValidationEngine()
    }
  }

  def parseRules(
      jsValue: JsValue,
      currentPath: JsPath = JsPath
  ): ValidationEngine = {
    ValidationEngine()
  }

  def traverseObj(
      obj: JsObject,
      currentContext: Set[String] = Set.empty,
      parseAsRules: Boolean = false
  ): ValidationEngine = {
    ValidationEngine()
  }

  def traverseArr(
      arr: JsArray,
      currentContext: Set[String] = Set.empty,
      parseAsRules: Boolean = false
  ): ValidationEngine = {
    ValidationEngine()
  }

  def getRules(jsValue: JsValue, currentPath: JsPath = JsPath) = {
    jsValue match {
      case obj: JsObject => obj
      case arr: JsArray  => arr
    }
  }

  def getRulesArr(jsArr: JsArray, currentPath: JsPath = JsPath) = {}

}
