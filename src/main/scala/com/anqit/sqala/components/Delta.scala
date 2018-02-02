package com.anqit.sqala.components

trait Delta[S <: State, A <: Action] {
    def apply(s: S, a: A): S
}
