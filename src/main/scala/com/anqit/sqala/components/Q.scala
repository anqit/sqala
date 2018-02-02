package com.anqit.sqala.components

import collection.mutable.Map

trait Q[S <: State, A <: Action] {
    def values: Map[(S, A), Double]

    def apply(s: S, a: A) = values((s, a))
    def update(s: S, a: A, reward: Double, gamma: Double, newState: S) = {
        values((s, a)) = newVal(reward, gamma, newState)
    }

    def newVal(reward: Double, gamma: Double, newState: S) = {
        val maxActionValue = values.filterKeys(k => k._1 == newState).values.max

        reward + gamma * maxActionValue
    }
}

object Q {
    def apply[S <: State, A <: Action](states: Set[S], actions: Set[A]) = {
        val values = Map[(S, A), Double]()
        states.foreach(s => actions.foreach(a => values((s, a)) = 0));

        new BasicQ(values)
    }
}

class BasicQ[S <: State, A <: Action](val values: Map[(S, A), Double]) extends Q[S, A]