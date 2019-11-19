import java.util

import play.api.libs.json.{JsArray, JsObject, JsPath, JsString, JsValue, Json}
import java.util.{List => JList}

class DocumentParser {
//
//  //val documentKeywords = Map("AND" -> getAndTestRule)
//  def parse(rulesDoc: String) = {
//    val jsVal = Json.parse(rulesDoc)
//  }
//
//  def traverseVal(jsVal: JsValue): JList[ContextTestRule] = {
//    traverseVal(jsVal, currentContext = Set.empty, parseAsRules = false)
//  }
//
//  def traverseVal(jsVal: JsValue,
//                  currentContext: Set[String],
//                  parseAsRules: Boolean): JList[ContextTestRule] = {
//    jsVal match {
//      case obj: JsObject => traverseObj(obj, currentContext, parseAsRules)
//      case arr: JsArray  => traverseArr(arr)
//      case _             => new util.ArrayList()
//    }
//  }
//
//  def traverseObj(obj: JsObject,
//                  currentContext: Set[String] = Set.empty,
//                  parseAsRules: Boolean = false): JList[ContextTestRule] = {
//    val list = new util.ArrayList[ContextTestRule](obj.fields.size)
//  }
//
//  def parseRules(rules: JsValue): TestRule = {
//    rules match {
//      case obj: JsObject =>
//      case arr: JsArray =>
//    }
//  }
//
//  def getRules(rulesObj: JsObject): JList[TestRule] = {
//
//  }
//
//  def getPrimitiveTestRule(obj: JsObject) = {
//  }
//  def getAndTestRule(value: JsValue): JList[TestRule] = {
//
//  }
//
//  def traverseArr(arr: JsArray,
//                  currentContext: Set[String] = Set.empty,
//                  parseAsRules: Boolean = false): JList[ContextTestRule] = {}

}
