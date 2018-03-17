package com.anqit.sqala.mdp

import com.anqit.sqala.components.{Action, State}

abstract class Q[S <: State, A <: Action] {
    def apply(s: S, a: A): Double
    def update(s: S, a: A, reward: Double, newState: S): Q[S, A]
    def policy: S => A
}

object Q {
    def q[S <: State, A <: Action](gamma: Double) = new TableLookupQ[S, A](gamma)
    def ndq[S <: State, A <: Action](gamma: Double) = new NonDeterministicQ[S, A](gamma)
}

class TableLookupQ[S <: State, A <: Action] private (gamma: Double, private var table: Map[S, Map[A, Double]]) extends Q[S, A] {
    require(gamma >= 0.0 && gamma < 1.0)
    def this(gamma: Double) = this(gamma, Map[S, Map[A, Double]]())

    override def apply(s: S, a: A) = {
        var actionsMap = getActionsMap(s)
        if(!actionsMap.contains(a))
            table = table + (s -> (actionsMap + (a -> 0.0)))

        table(s)(a)
    }

    override def update(s: S, a: A, reward: Double, newState: S) = {
        val curr = getActionsMap(s)
        new TableLookupQ[S, A](gamma, table + (s -> (curr + (a -> newValue(reward, newState)))))
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

class NonDeterministicQ[S <: State, A <: Action] private (gamma: Double, private var table: Map[S, Map[A, Double]], private var visits: Map[S, Map[A, Int]]) extends Q[S, A] {
    require(gamma >= 0.0 && gamma < 1.0)
    def this(gamma: Double) = this(gamma, Map[S, Map[A, Double]](), Map[S, Map[A, Int]]())

    override def apply(s: S, a: A) = {
        var actionsMap = getActionsMap(s)
        if(!actionsMap.contains(a))
            actionsMap += a -> 0.0
        table(s)(a)
    }

    override def update(s: S, a: A, reward: Double, newState: S) = {
        val curr = getActionsMap(s)
        val visitCount = visit(s, a)
        val alpha = 1.0 / (1.0 + visitCount)
        new NonDeterministicQ[S, A](gamma, table + (s -> (curr + (a -> newValue(reward, alpha, s, a, newState)))), visits + (s -> (getVisitsMap(s) + (a -> visitCount))))
    }

    override def policy = table.mapValues(v => v.maxBy(_._2)._1)

    private def newValue(reward: Double, alpha: Double, currentState: S, currentAction: A, newState: S) = {
        val maxQ = if(getActionsMap(newState).values.isEmpty) 0 else getActionsMap(newState).values.max

        (1 - alpha) * this(currentState, currentAction) + alpha * (reward + gamma * maxQ)
    }

    /**
      * "visit" the state-action pair and return the updated visit count
      * @param s
      * @param a
      * @return
      */
    private def visit(s: S, a: A) = {
        val visits = getVisitsMap(s)
        visits.getOrElse(a, 0) + 1
    }

    private def getActionsMap(s: S) = {
        if(!table.contains(s)) {
            table += s -> Map[A, Double]()
        }
        table(s)
    }

    private def getVisitsMap(s: S) = {
        if(!visits.contains(s)) {
            visits += s -> Map[A, Int]()
        }
        visits(s)
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
