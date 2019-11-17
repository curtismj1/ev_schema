import java.util
import java.util.function.Predicate

import scala.jdk.CollectionConverters._
import java.util.{List => JList}
import TestRuleFactory._
import java.util.stream.{Stream => JStream}

object ValidationEngine {

  def validate(rules: Iterable[ContextTestRule[TestRule]]) = {
    validateForActiveContext(rules.toVector, Set.empty)
  }

  def validate(rules: java.util.List[ContextTestRule[TestRule]]) = {
    validateForActiveContext(rules.asScala.toVector, Set.empty)
  }

  def validateForActiveContext(
      rules: Vector[ContextTestRule[TestRule]],
      activeContext: Set[String] = Set.empty): TestRule with Describable = {
      and(getActivatedRules(rules, activeContext).map(_.rule))
  }

  def getActivatedRules(rules: Vector[ContextTestRule[TestRule]],
                        activeContext: Set[String] = Set.empty)
    : JStream[ContextTestRule[TestRule]] = {
    val rulesByActive = rules.groupBy(contextRule =>
      (contextRule.requiresContext diff activeContext).isEmpty)
    val activeRules = rulesByActive.getOrElse(true, Vector.empty)
    val currentActiveList: JList[ContextTestRule[TestRule]] = activeRules.asJava
    val inactiveRules: Vector[ContextTestRule[TestRule]] = rulesByActive.getOrElse(false, Vector.empty)
    val groupedActiveRules = activeRules.groupBy(_.rule.isValid)
    val passed = groupedActiveRules.getOrElse(true, Vector.empty)
    //Just because a rule failed, doesn't mean the overall computation is to fail
    //because those rules may have just failed to activate some context.
    val failed = groupedActiveRules.getOrElse(false, Vector.empty)
    val failingRules = failed.filter(rule => ContextTestRule.shouldFail(rule))
    val newContext = passed.flatMap(_.activatesContext).toSet
    if ((activeContext diff newContext).nonEmpty) {
        activeRules.asJavaCollection
          .stream()
          .flatMap(_ => getActivatedRules(inactiveRules, newContext union activeContext))
    } else {
      currentActiveList.stream()
    }
  }

}
