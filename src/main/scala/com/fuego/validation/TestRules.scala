package com.fuego.validation

import java.util.function.Function

import play.api.libs.json.{JsPath, JsValue}

import scala.annotation.varargs

trait Describable {
  def description: String
}

trait ValidationReport extends Describable {
  def passed(): Boolean
  def failed(): Boolean            = !passed()
  override def description: String = ""
  def and(otherReport: ValidationReport): AndReport = this match {
    case a: AndReport => a.copy(a.reports :+ otherReport)
    case _            => AndReport(Vector(this, otherReport))
  }
  def or(otherReport: ValidationReport): OrReport = this match {
    case a: OrReport => a.copy(a.reports :+ otherReport)
    case _                     => OrReport(Vector(this, otherReport))
  }
}

object ValidationReport {
  def failed(desc: String): ValidationReport = new ValidationReport {
    override def passed(): Boolean   = false
    override def description: String = desc
  }
  def passed(desc: String): ValidationReport = new ValidationReport {
    override def passed(): Boolean   = true
    override def description: String = desc
  }

  def and(report: ValidationReport, report2: ValidationReport): AndReport =
    report.and(report2)
  def or(
      report: ValidationReport,
      report2: ValidationReport
  ): OrReport = report.or(report2)

  @varargs
  def and[A](
      functions: Function[A, ValidationReport]*
  ): Function[A, AndReport] =
    a => AndReport(functions.map(func => func(a)).toVector)

  @varargs
  def or[A](
      functions: Function[A, ValidationReport]*
  ): Function[A, OrReport] =
    a => OrReport(functions.map(func => func(a)).toVector)

}

case class JsonPathRule(
    extractionPath: JsPath,
    jsValueFunc: JsValue => ValidationReport,
    emptyFunc: ValidationReport
) extends TestRule[JsValue, ValidationReport] {
  override def validate(obj: JsValue): ValidationReport =
    extractionPath(obj).headOption
      .fold(emptyFunc)(jsValueFunc)
}
object JsonPathRule {
  def required(
      extractionPath: JsPath,
      jsValueFunc: JsValue => ValidationReport
  ): JsonPathRule = {
    JsonPathRule(
      extractionPath,
      jsValueFunc,
      ValidationReport.failed(
        s"Value at required path: ${extractionPath.toString()} does not exist."
      )
    )
  }
  def optional(
      extractionPath: JsPath,
      jsValueFunc: JsValue => ValidationReport
  ): JsonPathRule = {
    JsonPathRule(
      extractionPath,
      jsValueFunc,
      ValidationReport.passed(
        s"Value at optional path: ${extractionPath.toString()} does not exist."
      )
    )
  }
}

trait RequiresContext[+T] {
  def get: T
  def requiresContext: Set[String]
}

trait ActivatesContext[+T] {
  def get: T
  def activatesContext: Set[String]
}

trait RequiresActivatesContext[+T]
    extends RequiresContext[T]
    with ActivatesContext[T]

case class ContextWrapper[+T](
    private val item: T,
    requiresContext: Set[String] = Set.empty,
    activatesContext: Set[String] = Set.empty
) extends RequiresActivatesContext[T] {
  def get: T = item
}

case class ContextValidationReport[T <: ValidationReport](
    rule: T,
    requiresContext: Set[String] = Set.empty,
    activatesContext: Set[String] = Set.empty,
    isFailingReport: Option[Boolean] = None
) extends RequiresActivatesContext[T] {
  def get: T = rule
}

object ContextValidationReport {
  def shouldFail[T <: ValidationReport](
      implicit rule: ContextValidationReport[T]
  ): Boolean = {
    rule.isFailingReport.getOrElse(
      rule.activatesContext.isEmpty
    )
  }
}

abstract class ReduceReport(val reports: Vector[ValidationReport])
    extends ValidationReport

case class AndReport(override val reports: Vector[ValidationReport])
    extends ReduceReport(reports = reports) {
  override def passed(): Boolean = reports.forall(_.passed())

}

case class OrReport(override val reports: Vector[ValidationReport])
    extends ReduceReport(reports = reports) {
  override def passed(): Boolean = reports.exists(_.passed())
}

case class RegexValidationReportSupplier(regex: String)
    extends TestRule[String, RegexValidationReport] {
  override def validate(str: String): RegexValidationReport =
    RegexValidationReport(regex, str)
}

case class RegexValidationReport(regex: String, str: String)
    extends ValidationReport {
  override def description: String =
    s"Regex pattern ${regex} expected to match $str"
  override def passed(): Boolean = regex.r.findFirstIn(str).isDefined
}

case class StringEqualsTestRule(expected: String)
    extends TestRule[String, StringEqualsValidationReport] {
  override def validate(actual: String): StringEqualsValidationReport =
    StringEqualsValidationReport(expected, actual)
}

case class StringEqualsValidationReport(expected: String, actual: String)
    extends ValidationReport {
  override def description: String =
    s"${actual} == ${expected}"
  override def passed(): Boolean = expected.equals(actual)
}

case class StringContainsTestRule(expected: Vector[String])
    extends TestRule[String, StringContainsValidationReport] {
  override def validate(actual: String): StringContainsValidationReport =
    StringContainsValidationReport(expected, actual)
}

case class StringContainsValidationReport(
    expected: Vector[String],
    actual: String
) extends ValidationReport {

  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = missingSubstrings.isEmpty
  lazy val missingSubstrings = expected
    .filter(substr => !actual.contains(substr))
}

case class StringOrContainsValidationReportSupplier(expected: Vector[String])
    extends Function[String, StringOrContainsValidationReport] {
  override def apply(actual: String): StringOrContainsValidationReport =
    StringOrContainsValidationReport(expected, actual)
}

case class StringOrContainsValidationReport(
    expected: Vector[String],
    actual: String
) extends ValidationReport {
  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = !foundSubstrings.isEmpty
  lazy val foundSubstrings = expected
    .filter(substr => actual.contains(substr))
}

trait TestRule[-T, +U <: ValidationReport] extends (T => U) {
  def validate(t: T): U
  override def apply(t: T): U = validate(t)
}

object RuleReducers {
  def and[T]: TestRuleReducer[T, AndReport] = rules => AndTestRule[T](rules.toVector)
  def or[T]: TestRuleReducer[T, OrReport] =  rules => OrTestRule[T](rules.toVector)
}

case class AndTestRule[T](rules: Vector[TestRule[T, ValidationReport]] = Vector())
    extends TestRule[T, AndReport] {
  override def validate(t: T): AndReport = {
    val reports: Vector[ValidationReport] =
      rules.map(rule => rule.validate(t))
    AndReport(reports)
  }
}

case class OrTestRule[T](rules: Vector[TestRule[T, ValidationReport]])
    extends TestRule[T, OrReport] {
  override def validate(t: T): OrReport = {
    val reports: Vector[ValidationReport] =
      rules.map(rule => rule.validate(t))
    OrReport(reports)
  }
}

object ValidationReportCombinators {
  @varargs
  def and[T](rules: TestRule[T, ValidationReport]*): AndTestRule[T] =
    AndTestRule(rules.toVector)

  @varargs
  def or[T](rules: TestRule[T, ValidationReport]*): OrTestRule[T] =
    OrTestRule(rules.toVector)

}
