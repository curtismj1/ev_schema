package com.fuego.validation

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import play.api.libs.json.jackson.PlayJsonModule
import play.api.libs.json.{Json, Writes, _}

object TestRuleSerde {}
//
//class ReportSerde(objectMapper: ObjectMapper = new ObjectMapper()) {
//
//  objectMapper.registerModule(DefaultScalaModule)
//  objectMapper.registerModule(new PlayJsonModule(JsonParserSettings()))
//
//  implicit val reportWrites: Writes[ValidationReport] = {
//    case andReport: AndReport => andWrites.writes(andReport)
//    case orReport: OrReport   => orWrites.writes(orReport)
//    case report => report.serialize
////      val baseReport = objectMapper.convertValue(report, classOf[JsObject]) +
////        ("passed", JsBoolean(report.passed))
////      if (report.description.isEmpty) baseReport
////      else baseReport + ("description", JsString(report.description))
//
//  }
//
//  implicit lazy val andWrites: Writes[AndReport] =
//    report =>
//      Json.obj(
//        "AND"    -> report.reports.map(Json.toJson[ValidationReport]),
//        "passed" -> report.passed()
//      )
//  implicit lazy val orWrites: Writes[OrReport] =
//    report => {
//      val rep = report.reports.map(Json.toJson[ValidationReport])
//      val reducedObjects = rep
//        .foldLeft(Json.obj()) { (partial, next) =>
//          next match {
//            case a: JsObject =>
//              partial.deepMerge(a)
//            case _ => partial
//          }
//        }
//      if (reducedObjects.keys.size != rep.size)
//        Json.obj("OR"     -> rep,
//          "passed" -> report.passed())
//      else  Json.obj(
//        "OR"     -> reducedObjects,
//        "passed" -> report.passed()
//      )
//    }
//
//}
