package com.fuego

package object validation {

  type TestRuleReducer[T, Report <: ValidationReport] = Seq[TestRule[T, ValidationReport]] => TestRule[T, Report]

//  implicit class ListReducers(implicit rules: List[TestRule[_, _]]) {
//    def and()
//  }

}
