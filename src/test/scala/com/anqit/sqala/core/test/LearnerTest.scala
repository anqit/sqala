package com.anqit.sqala.core.test

import com.anqit.sqala.core.test.mdp.deterministic.{DeterministicGridMdp, Move, Tile}
import com.anqit.sqala.mdp.Q
import org.scalatest.{BeforeAndAfter, FunSuite}

class LearnerTest extends FunSuite with BeforeAndAfter {

    test("basic") {
        val mdp = DeterministicGridMdp()
        mdp.learn()
        mdp.printQ
        mdp.printPolicy
    }

    test("non-deterministic") {
        val q = Q.ndq[Tile, Move](.9)

    }

}
