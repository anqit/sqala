package com.anqit.sqala.mdp

import com.anqit.sqala.components.{Action, State}

import scala.util.Random

trait Agent[S <: State, A <: Action] {
    def update(s: S, a: A, reward: Double, newState: S): Agent[S, A]
    def policy: S => A
    def selectAction(s: S): A
    def q: Q[S, A]
}

object Agent {
    def qAgent[S <: State, A <: Action](q: Q[S, A], actionSelector: S => A) = new QAgent(q, actionSelector)

    def randomActionSelector[S <:State, A <: Action](as: A*) = (_: S) => as(Random.nextInt(as.size))
}

class QAgent[S <: State, A <: Action](val q: Q[S, A], actionSelector: S => A) extends Agent[S, A] {
    override def update(s: S, a: A, reward: Double, newState: S) = Agent.qAgent(q.update(s, a, reward, newState), actionSelector)
    override def policy = q.policy
    override def selectAction(s: S) = actionSelector(s)
}