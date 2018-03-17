package com.anqit.sqala.core.test

import com.anqit.sqala.core.test.mdp.deterministic.DeterministicGridMdp
import com.anqit.sqala.core.test.mdp.nondeterministic.NonDeterministicGridMdp
import org.scalatest.{BeforeAndAfter, FunSuite}

class LearnerTest extends FunSuite with BeforeAndAfter {

    test("basic") {
        val mdp = DeterministicGridMdp().learn()
        mdp.printQ
        mdp.printPolicy
    }

    test("non-deterministic") {
        val mdp = NonDeterministicGridMdp().learn()
        mdp.printQ
        mdp.printPolicy
    }

}
