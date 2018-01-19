package com.anqit.sqala.components

import scala.collection.mutable.Map

class Q(states: Set[State], gamma: Double) {
    require(gamma >= 0 && gamma < 1)

    val values = Map[(State, Action), Double]().withDefaultValue(0)

    def getValue(s: State, a: Action): Double = values((s, a))

    def updateValue(s: State, a: Action, r: Double, s2: State): Double = {
        try {
            val bestKnownMove = values.filterKeys(k => k._1 == s).values.max
            val updatedScore = r + gamma * bestKnownMove

            values((s, a)) = updatedScore

            updatedScore
        }
    }
}
