package com.fuego

package object validation {

  type TestRuleFunctionCombinator[A, B <: TestRule] = collection.Seq[A => TestRule] => A => B
  implicit class CombinatorOps[A](f: A => TestRule) {
    def and(f2: A => TestRule): A => AndTestRule =
       TestRuleCombinators.and(f, f2)
    def or(f2: A => TestRule): A => OrTestRule =
      TestRuleCombinators.or(f, f2)
  }

}
