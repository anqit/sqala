package com.anqit.sqala.mdp

import com.anqit.sqala.components.{Action, State}
import com.anqit.sqala.core.Learner

class MDP[S <: State, A <: Action](agent: Agent[S, A], environment: Environment[S, A]) {
    def learn() = Learner.learn(agent, environment)
}
