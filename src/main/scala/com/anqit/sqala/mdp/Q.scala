package com.anqit.sqala.mdp

import com.anqit.sqala.components.{Action, State}

abstract class Q[S <: State, A <: Action] {
    def apply(s: S, a: A): Double
    def update(s: S, a: A, reward: Double, newState: S): Q[S, A]
    def policy: S => A
}

object Q {
    def q[S <: State, A <: Action](gamma: Double) = new TableLookupQ[S, A](gamma)
}

class TableLookupQ[S <: State, A <: Action] private (gamma: Double, private var table: Map[S, Map[A, Double]]) extends Q[S, A] {
    require(gamma >= 0.0 && gamma < 1.0)
    def this(gamma: Double) = this(gamma, Map[S, Map[A, Double]]())

    override def apply(s: S, a: A) = {
        var actionsMap = getActionsMap(s)
        if(!actionsMap.contains(a))
            actionsMap += a -> 0.0
        table(s)(a)
    }

    override def update(s: S, a: A, reward: Double, newState: S) = {
        val curr = getActionsMap(s)
        new TableLookupQ(gamma, table + (s -> (curr + (a -> newValue(reward, newState)))))
    }

    override def policy = table.mapValues(v => v.maxBy(_._2)._1)

    private def newValue(reward: Double, newState: S) = {
        val maxQ = if(getActionsMap(newState).values.isEmpty) 0 else getActionsMap(newState).values.max

        reward + gamma * maxQ
    }

    private def getActionsMap(s: S) = {
        if(!table.contains(s)) {
            table += s -> Map[A, Double]()
        }
        table(s)
    }

    override def toString: String = {
        val b = new StringBuilder
        for ((s, as) <- table) {
            b ++= s + "\n"
            for((a, v) <- as) {
                b ++= "\t" + a + " -> " + v
            }
            b ++= "\n"
        }

        b.toString
    }
}
