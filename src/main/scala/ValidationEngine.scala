import scala.jdk.CollectionConverters._
import java.util.{List => JList}
import java.util.stream.{Collectors, Stream => JStream}
import play.api.libs.json.{JsObject, JsPath, JsValue}


case class ValidationEngine(activeContext: Set[String] = Set.empty,
                            ruleForPath: Map[JsPath, JsValue => TestRule] =
                              Map.empty) {
  def withContext(context: Set[String]): ValidationEngine =
    copy(activeContext = context)
  def withAdditionalContext(context: Set[String]): ValidationEngine =
    copy(activeContext = activeContext union context)
  def withRuleForPath(path: JsPath,
                      ruleFunc: JsValue => TestRule,
                      combinator: TestRuleFunctionCombinator[JsValue] =
                        TestRule.and[JsValue]): ValidationEngine = {
    val updatedFunc = ruleForPath.get(path) match {
      case Some(existingFunction) => combinator(existingFunction, ruleFunc)
      case None                   => ruleFunc
    }
    copy(ruleForPath = ruleForPath.updated(path, updatedFunc))
  }

  def validate(input: JsValue): TestRule = {
    ruleForPath.keys.foldLeft(TestRule.passed(""))((partialRule, jsPath) => {
      partialRule.and(ruleForPath(jsPath)(jsPath(input).head))
    })
  }
}

/*
engine.withNewRule(RuleBuilder.forPath(jsPath).activatesContext(context)
      .requiresContext(somContext)
 */

object ValidationEngine {

  def validate(rules: Iterable[ContextTestRule[TestRule]]): AndTestRule = {
    validateForActiveContext(rules.toVector, Set.empty)
  }

  def validate(
      rules: java.util.List[ContextTestRule[TestRule]]): AndTestRule = {
    validateForActiveContext(rules.asScala.toVector, Set.empty)
  }

  def validateForActiveContext(
      rules: Vector[ContextTestRule[TestRule]],
      activeContext: Set[String] = Set.empty): AndTestRule = {
    AndTestRule(
      getActivatedRules(rules, activeContext)
        .stream()
        .map(_.rule)
        .collect(Collectors.toList()))
  }

  def getActivatedRules(rules: Vector[ContextTestRule[TestRule]],
                        activeContext: Set[String] = Set.empty)
    : JList[ContextTestRule[TestRule]] = {
    val rulesByActive = rules.groupBy(contextRule =>
      (contextRule.requiresContext diff activeContext).isEmpty)
    val activeRules = rulesByActive.getOrElse(true, Vector.empty)
    val currentActiveList: JList[ContextTestRule[TestRule]] = activeRules.asJava
    val inactiveRules: Vector[ContextTestRule[TestRule]] =
      rulesByActive.getOrElse(false, Vector.empty)
    val groupedActiveRules = activeRules.groupBy(_.rule.passed())
    val passed = groupedActiveRules.getOrElse(true, Vector.empty)
    //Just because a rule failed, doesn't mean the overall computation is to fail
    //because those rules may have just failed to activate some context.
    val failed = groupedActiveRules.getOrElse(false, Vector.empty)
    val failingRules = failed.filter(rule => ContextTestRule.shouldFail(rule))
    val newContext = passed.flatMap(_.activatesContext).toSet
    if ((activeContext diff newContext).nonEmpty) {
      activeRules.asJavaCollection
        .stream()
        .flatMap(_ =>
          getActivatedRules(inactiveRules, newContext union activeContext)
            .stream())
        .collect(Collectors.toList())
    } else {
      currentActiveList
    }
  }

}
