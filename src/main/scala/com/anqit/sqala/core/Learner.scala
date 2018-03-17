package com.anqit.sqala.core

import com.anqit.sqala.components.{Action, State}
import com.anqit.sqala.mdp.{Agent, Environment}

object Learner {
    def learn[S <: State, A <: Action](a: Agent[S, A], e: Environment[S, A], numEpisodes: Int = 10000): Agent[S, A] = {
        if(numEpisodes > 0) learn(runEpisode(a, e), e, numEpisodes - 1) else a
    }

    private def runEpisode[S <: State, A <: Action](a: Agent[S, A], e: Environment[S, A]): Agent[S, A] = {
        def runEpisode[S <: State, A <: Action](a: Agent[S, A], e: Environment[S, A], curr: S): Agent[S, A] = {
            val action = a.selectAction(curr)
            val nextState = e.delta(curr, action)
            val reward = e.reward(curr, action, nextState)
            val newAgent = a.update(curr, action, reward, nextState)

            if(e.isTerminal(curr)) {
                newAgent
            }
            else {
                runEpisode(newAgent, e, nextState)
            }
        }

        runEpisode(a, e, e.start)
    }
}
