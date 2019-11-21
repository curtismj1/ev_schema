import scala.jdk.CollectionConverters._
import java.util.{List => JList}
import java.util.stream.{Collectors, Stream => JStream}
import play.api.Logger
import play.api.libs.json.{JsObject, JsPath, JsValue}

case class ValidationEngineTestRuleBuilder(ruleFunc: JsValue => TestRule)(
    implicit validationEngine: ValidationEngine
) {
  def forPath(path: JsPath): ValidationEngineRule =
    ValidationEngineRule(ruleFunc, path)
  def forRootPath(): ValidationEngineRule =
    ValidationEngineRule(ruleFunc, JsPath)
}
case class ValidationEngineRule(
    ruleFunc: JsValue => TestRule,
    path: JsPath,
    requiresContext: Set[String] = Set.empty,
    activatesContext: Set[String] = Set.empty
)(implicit validationEngine: ValidationEngine) {

  def withRequiresContext(requiresContext: Set[String]): ValidationEngineRule =
    copy(requiresContext = requiresContext)
  def withActivatesContext(
      activatesContext: Set[String]
  ): ValidationEngineRule =
    copy(activatesContext = activatesContext)
  def withRequiresContext(requiresContext: String*): ValidationEngineRule =
    withRequiresContext(requiresContext.toSet)
  def withActivatesContext(activatesContext: String*): ValidationEngineRule =
    withActivatesContext(activatesContext.toSet)
  def add: ValidationEngine =
    validationEngine.copy(engineRules = validationEngine.engineRules :+ this)
}

case class ValidationEngine(
    activeContext: Set[String] = Set.empty,
    engineRules: Vector[ValidationEngineRule] = Vector.empty
) {

  val logger: Logger = Logger(this.getClass)

  def withContext(context: Set[String]): ValidationEngine =
    copy(activeContext = context)
  def withAdditionalContext(context: Set[String]): ValidationEngine =
    copy(activeContext = activeContext union context)
  def withRuleFunc(
      ruleFunc: JsValue => TestRule
  ): ValidationEngineTestRuleBuilder =
    ValidationEngineTestRuleBuilder(ruleFunc)(this)

  def validate(input: JsValue): TestRule = {
    val rulesByActive = engineRules
      .groupBy(rule => (rule.requiresContext diff activeContext).isEmpty)
      .withDefault(_ => Vector.empty)
    val activeRules   = rulesByActive(true)
    val inactiveRules = rulesByActive(false)
    val activeTestRules = activeRules.map(engineRule => {
      val testRule =
        JsonObjTestRuleSupplier(engineRule.path, engineRule.ruleFunc)
          .apply(input)
      logger.info(
        s"Test rule for path: '${engineRule.path.toJsonString}' is ${testRule.passed()}."
      )
      logger.info(s"Description ${testRule.description}")
      (engineRule, testRule)
    })
    val newContext = activeContext union
      activeTestRules.foldLeft(Set.empty[String]) {
        case (partial, (engineRule, testRule)) =>
          if (testRule.passed()) {
            val newRuleContext = engineRule.activatesContext diff activeContext
            if (newRuleContext.nonEmpty) {
              logger.info(
                s"New contexts activated: ${newRuleContext.mkString(", ")}"
              )
            }
            partial union engineRule.activatesContext
          } else {
            partial
          }
      }
    val finalRule = AndTestRule(activeTestRules.map(_._2).toBuffer.asJava)
    if (newContext.size != activeContext.size) {
      logger.info("-----------------------------------------------------")
      finalRule and copy(
        activeContext = newContext,
        engineRules = inactiveRules
      ).validate(input)
    } else {
      finalRule
    }
  }
}

object ValidationEngine {

  def validate(rules: Iterable[ContextTestRule[TestRule]]): AndTestRule = {
    validateForActiveContext(rules.toVector, Set.empty)
  }

  def validate(
      rules: java.util.List[ContextTestRule[TestRule]]
  ): AndTestRule = {
    validateForActiveContext(rules.asScala.toVector, Set.empty)
  }

  def validateForActiveContext(
      rules: Vector[ContextTestRule[TestRule]],
      activeContext: Set[String] = Set.empty
  ): AndTestRule = {
    AndTestRule(
      getActivatedRules(rules, activeContext)
        .stream()
        .map(_.rule)
        .collect(Collectors.toList())
    )
  }

  def getActivatedRules(
      rules: Vector[ContextTestRule[TestRule]],
      activeContext: Set[String] = Set.empty
  ): JList[ContextTestRule[TestRule]] = {
    val rulesByActive = rules.groupBy(
      contextRule => (contextRule.requiresContext diff activeContext).isEmpty
    )
    val activeRules = rulesByActive.getOrElse(true, Vector.empty)
    val currentActiveList: JList[ContextTestRule[TestRule]] =
      activeRules.toBuffer.asJava
    val inactiveRules: Vector[ContextTestRule[TestRule]] =
      rulesByActive.getOrElse(false, Vector.empty)
    val groupedActiveRules = activeRules.groupBy(_.rule.passed())
    val passed             = groupedActiveRules.getOrElse(true, Vector.empty)
    //Just because a rule failed, doesn't mean the overall computation is to fail
    //because those rules may have just failed to activate some context.
    val failed       = groupedActiveRules.getOrElse(false, Vector.empty)
    val failingRules = failed.filter(rule => ContextTestRule.shouldFail(rule))
    val newContext   = passed.flatMap(_.activatesContext).toSet
    if ((activeContext diff newContext).nonEmpty) {
      activeRules.asJavaCollection
        .stream()
        .flatMap(
          _ =>
            getActivatedRules(inactiveRules, newContext union activeContext)
              .stream()
        )
        .collect(Collectors.toList())
    } else {
      currentActiveList
    }
  }

}
