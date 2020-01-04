package com.fuego.validation

import java.util
import java.util.stream.Collectors
import java.util.{List => JList}

import play.api.libs.json.{JsPath, JsValue}

import scala.annotation.varargs
import scala.jdk.CollectionConverters._

trait Describable {
  def description: String
}

trait TestRule extends Describable {
  def passed(): Boolean
  def failed(): Boolean            = !passed()
  override def description: String = ""
  def and(otherRule: TestRule): AndTestRule = this match {
    case a: AndTestRule => a.rules.add(otherRule); a
    case _              => AndTestRule(util.Arrays.asList(this, otherRule))
  }
  def or(otherRule: TestRule): OrTestRule = this match {
    case a: OrTestRule => a.rules.add(otherRule); a
    case _             => OrTestRule(util.Arrays.asList(this, otherRule))
  }
}

object TestRule {
  def failed(desc: String): TestRule = new TestRule {
    override def passed(): Boolean   = false
    override def description: String = desc
  }
  def passed(desc: String): TestRule = new TestRule {
    override def passed(): Boolean   = true
    override def description: String = desc
  }

  def and(rule1: TestRule, rule2: TestRule): AndTestRule = rule1.and(rule2)
  def or(rule1: TestRule, rule2: TestRule): OrTestRule   = rule1.or(rule2)

  @varargs
  def and[A](funcs: Function[A, TestRule]*): Function[A, AndTestRule] =
    a => AndTestRule(funcs.map(func => func(a)).toBuffer.asJava)

  @varargs
  def or[A](funcs: Function[A, TestRule]*): Function[A, OrTestRule] =
    a => OrTestRule(funcs.map(func => func(a)).toBuffer.asJava)

}

case class JsonObjTestRuleSupplier(
    extractionPath: JsPath,
    jsValueFunc: JsValue => TestRule
) extends Function[JsValue, TestRule] {
  override def apply(obj: JsValue): TestRule = {
    val test = extractionPath(obj)
    test.headOption.fold(
      TestRule.failed(s"Value at path $extractionPath is undefined")
    )(jsValueFunc)
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

case class ContextTestRule[T <: TestRule](
    rule: T,
    requiresContext: Set[String] = Set.empty,
    activatesContext: Set[String] = Set.empty,
    isFailingRule: Option[Boolean] = None
) extends RequiresActivatesContext[T] {
  def get: T = rule
}

object ContextTestRule {
  def shouldFail[T <: TestRule](implicit rule: ContextTestRule[T]): Boolean = {
    rule.isFailingRule.getOrElse(
      rule.activatesContext.isEmpty
    )
  }
}

abstract class ReduceTestRule(val rules: JList[TestRule]) extends TestRule

case class AndTestRule(override val rules: JList[TestRule])
    extends ReduceTestRule(rules = rules) {
  override def passed(): Boolean = rules.stream().allMatch(_.passed())
  override def description: String =
    s"AND (\n\t${rules.stream().map(_.description).collect(Collectors.joining("\n\t"))}\n)"
}

case class OrTestRule(override val rules: JList[TestRule])
    extends ReduceTestRule(rules = rules) {
  override def passed(): Boolean = rules.stream().anyMatch(_.passed())
  override def description: String =
    s"OR (\n\t${rules.stream().map(_.description).collect(Collectors.joining("\n\t"))}\n)"
}

case class RegexTestRuleSupplier(regex: String)
    extends Function[String, RegexTestRule] {
  override def apply(str: String): RegexTestRule = RegexTestRule(regex, str)
}

case class RegexTestRule(regex: String, str: String) extends TestRule {
  override def description: String =
    s"Regex pattern ${regex} expected to match $str"
  override def passed(): Boolean = regex.r.findFirstIn(str).isDefined
}

case class StringEqualsTestRuleSupplier(expected: String)
    extends Function[String, StringEqualsTestRule] {
  override def apply(actual: String): StringEqualsTestRule =
    StringEqualsTestRule(expected, actual)
}

case class StringEqualsTestRule(expected: String, actual: String)
    extends TestRule {
  override def description: String =
    s"String ${actual} expected to match $expected"
  override def passed(): Boolean = expected.equals(actual)
}

case class StringContainsTestRuleSupplier(expected: JList[String])
    extends Function[String, StringContainsTestRule] {
  override def apply(actual: String): StringContainsTestRule =
    StringContainsTestRule(expected, actual)
}

case class StringContainsTestRule(expected: JList[String], actual: String)
    extends TestRule {

  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = missingSubstrings.isEmpty
  lazy val missingSubstrings = expected
    .stream()
    .filter(substr => !actual.contains(substr))
    .collect(Collectors.toList())

}

case class StringOrContainsTestRuleSupplier(expected: JList[String])
    extends Function[String, StringOrContainsTestRule] {
  override def apply(actual: String): StringOrContainsTestRule =
    StringOrContainsTestRule(expected, actual)
}

case class StringOrContainsTestRule(expected: JList[String], actual: String)
    extends TestRule {
  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = !foundSubstrings.isEmpty
  lazy val foundSubstrings = expected
    .stream()
    .filter(substr => actual.contains(substr))
    .collect(Collectors.toList())
}

object TestRuleCombinators {
  @varargs
  def and[T](functions: (T => TestRule)*): T => AndTestRule =
      t => AndTestRule(functions.map(_(t)).asJava)

  @varargs
  def or[T](functions: (T => TestRule)*): T => OrTestRule =
    t => OrTestRule(functions.map(_(t)).asJava)

  def and[T]: TestRuleFunctionCombinator[T, AndTestRule] = functions => and(functions = functions.toArray:_*)

  def or[T]: TestRuleFunctionCombinator[T, OrTestRule] = functions => or(functions.toArray:_*)


}
