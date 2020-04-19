package com.fuego.validation

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import play.api.libs.json.jackson.PlayJsonModule
import play.api.libs.json.{Json, Writes, _}

object TestRuleSerde {}

object ReportSerde {
  val objectMapper: ObjectMapper = new ObjectMapper()
  objectMapper.registerModule(DefaultScalaModule)
  objectMapper.registerModule(new PlayJsonModule(JsonParserSettings()))

  implicit val reportWrites: Writes[ValidationReport] = {
    case andReport: AndReport => andWrites.writes(andReport)
    case orReport: OrReport => orWrites.writes(orReport)
    case report =>
      objectMapper.convertValue(report, classOf[JsObject]) ++ Json.obj(
        "passed" -> report.passed,
        "description" -> report.description
      )
  }

  implicit lazy val andWrites: Writes[AndReport] =
    report =>
      Json.obj(
        "AND"    -> report.reports.map(Json.toJson[ValidationReport]),
        "passed" -> report.passed()
      )
  implicit lazy val orWrites: Writes[OrReport] =
    report =>
      Json.obj(
        "OR"     -> report.reports.map(Json.toJson[ValidationReport]),
        "passed" -> report.passed()
      )
}
