package com.anqit.sqala.components

class MDP[S <: State, A <: Action](states: Set[S], actions: Set[A], r: Reward[S, A], d: Delta[S, A], start: S, gamma: Double) {

}
