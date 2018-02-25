package com.anqit.sqala.components

import scala.util.Random

class Runner[S <: State, A <: Action](states: Set[S], actions: Set[A], r: Reward[S, A], d: Delta[S, A], gamma: Double) {
    require(gamma >= 0 && gamma < 1)

    val q = Q(states, actions)

    def run(): Q[S, A] = {
        var i = 0;

        while(i < 100) {
            var s = getState

            var j = 0;
            while(j < 100) {
                val a = getAction
                val next = d(s, a)
                val reward = r(s, a)

                q.update(s, a, reward, gamma, next)

                s = next
                j += 1
            }

            i += 1
        }

        q
    }

    def getAction = actions.toList(new Random().nextInt(actions.size))

    def getState = states.toList(new Random().nextInt(states.size))
}
