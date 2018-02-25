package com.anqit.sqala.mdp

import com.anqit.sqala.components.{Action, State}

trait Environment[S <: State, A <: Action]{
    def reward(s: S, a: A): Double
    def delta(s: S, a: A): S
    def start: S
    def isTerminal(s: S): Boolean
}

object Environment {
    def env[S <: State, A <: Action](r: (S, A) => Double, d: (S, A) => S, startState: S, t: S => Boolean) =
        new Environment[S, A] {
            override def delta(s: S, a: A): S = d(s, a)
            override def reward(s: S, a: A): Double = r(s, a)
            override def start = startState
            override def isTerminal(s: S): Boolean = t(s)
        }
}