package com.anqit.sqala.components

trait Reward[S <: State, A <: Action] {
    def apply(s: S, a: A): Double
}
