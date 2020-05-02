package com.fuego

import java.io.Serializable

import play.api.libs.json.{JsObject, JsPath, JsValue, Json}

package object validation {

  type TestRuleReducer[T, Report <: ValidationReport] = Seq[TestRule[T, ValidationReport]] => TestRule[T, Report]

//  implicit class ListReducers(implicit rules: List[TestRule[_, _]]) {
//    def and()
//  }

  implicit class WithCustomSerializer[T](obj: T) {
  }

  implicit class PathToObj(jsPath: JsPath) {

  }

}
