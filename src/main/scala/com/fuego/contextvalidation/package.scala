package com.fuego

import java.io.Serializable

import play.api.libs.json.{JsObject, JsPath, JsValue, Json}

package object contextvalidation {

  type TestRuleReducer[T, Report <: ValidationReport] = Seq[TestRule[T, ValidationReport]] => TestRule[T, Report]


}
