import java.util.function.Predicate
import java.util.stream.Collectors
import java.util.{List => JList}

import scala.jdk.CollectionConverters._

trait Describable {
  def description: String
}

trait TestRule extends Describable {
  def passed(): Boolean
  def failed(): Boolean = !passed()
  override def description: String = ""
}

case class ContextTestRule[T <: TestRule](
    rule: T,
    requiresContext: Set[String] = Set.empty,
    activatesContext: Set[String] = Set.empty,
    isFailingRule: Option[Boolean] = None)

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
  override def description: String = "AND"
}

case class OrTestRule(override val rules: JList[TestRule])
    extends ReduceTestRule(rules = rules) {
  override def passed(): Boolean = rules.stream().anyMatch(_.passed())
  override def description: String = "OR"
}

case class RegexTestRuleSupplier(regex: String)
    extends Function[String, RegexTestRule] {
  override def apply(str: String): RegexTestRule = RegexTestRule(regex, str)
}

case class RegexTestRule(regex: String, str: String) extends TestRule {
  override def description: String =
    s"Regex pattern ${regex} expected to match $str"
  override def passed(): Boolean = str.matches(regex)
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
  override def apply(actual: String): StringContainsTestRule
  = StringContainsTestRule(expected, actual)
}

case class StringContainsTestRule(expected: JList[String], actual: String)
    extends TestRule {

  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = missingSubstrings.isEmpty
  lazy val missingSubstrings = expected
    .stream()
    .filter(substr => actual.contains(substr))
    .collect(Collectors.toList())

}

case class StringOrContainsTestRuleSupplier(expected: JList[String])
    extends Function[String, StringOrContainsTestRule] {
}

case class StringOrContainsTestRule(expected: JList[String], actual: String)
    extends TestRule {
  override def description: String =
    s"$actual expected to contain all substrings"
  override def passed = foundSubstrings.isEmpty == false
  lazy val foundSubstrings = expected
    .stream()
    .filter(substr => actual.contains(substr))
    .collect(Collectors.toList())
}
