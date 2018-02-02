package com.anqit.sqala.components

import scala.util.Random

class Runner[S <: State, A <: Action](states: Set[S], actions: Set[A], r: Reward[S, A], d: Delta[S, A], start: S, gamma: Double) {
    require(gamma >= 0 && gamma < 1)

    val q = Q(states, actions)

    var s = start;

    def run(): Q[S, A] = {
        var i = 0;
        while(i < 100) {
            val a = getAction
            val next = d(s, a)
            val reward = r(s, a)

            q.update(s, a, reward, gamma, next)

            s = next
            i += 1
        }

        q
    }

    def getAction(): A = {
        actions.toList(new Random().nextInt(actions.size))
    }
}
