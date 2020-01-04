package com.fuego.validation

import com.fuego.validation.TestRuleCombinators.and
import play.api.libs.json
import play.api.libs.json._

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
    baseKeywordMap: Map[String, JsValue => String => TestRule] =
      RuleDocumentParser.defaultKeywordMap,
    ruleCombinator: TestRuleFunctionCombinator[String, TestRule] = TestRuleCombinators.and,
    currentPathExtractor: JsPath = JsPath
) extends Parser[String => TestRule] {

  def parse(rulesDoc: String): String => TestRule = {
    val jsVal = Json.parse(rulesDoc)
    parseVal(jsVal)
  }

  def keywordMap: Map[String, JsValue => String => TestRule] =
    baseKeywordMap.withDefault(str => copy(currentPathExtractor = currentPathExtractor \ str).parseVal _) ++
      Map(
        "AND" -> copy(ruleCombinator = TestRuleCombinators.and).parseVal,
        "OR" ->  copy(ruleCombinator = TestRuleCombinators.or).parseVal
      )

  def parseVal(jsVal: JsValue): String => TestRule =
    jsVal match {
      case jsObj: JsObject => parseObj(jsObj)
      case jsArr: JsArray  => parseArr(jsArr)
      case _ =>
        _ =>
          TestRule.failed("Could not find test rule")
    }

  //Should this iterate through all the key values to extract what is in the map and then recurse down?
  //How should it handle all the paths?
  def parseObj(jsObj: JsObject): String => TestRule = {
    val rules  =
      for {
        (key, value) <- jsObj.fields
        keywordFunc  <- keywordMap.get(key)
      } yield keywordFunc(value)
    ruleCombinator(rules)
  }

  def parseArr(jsArr: JsArray): String => TestRule =
    ruleCombinator(jsArr.value.map(jsVal => parseVal(jsVal)))
}

case class RuleDocumentParserBuilder(keywordMap: Map[String, JsValue => String => TestRule]) {
  def withBaseKeywordMap(keywordMap: Map[String, JsValue => String => TestRule]): RuleDocumentParserBuilder = {
    this.copy(keywordMap)
  }
}

object RuleDocumentParser {

  // TODO: Come back to this and think about how to avoid the casting issues
  val defaultKeywordMap: Map[String, JsValue => String => TestRule] = Map(
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
