package com.anqit.sqala.core.test.mdp.deterministic

import com.anqit.sqala.core.Learner
import com.anqit.sqala.core.test.mdp.{Down, GridMdp, Left, Move, Right, Tile, Up}
import com.anqit.sqala.mdp.{Agent, Environment, Q}

class DeterministicGridMdp private(agent: Agent[Tile, Move], environment: Environment[Tile, Move], board: Array[Array[Tile]]) extends GridMdp(agent, environment, board) {
    override def learn() = new DeterministicGridMdp(Learner.learn(agent, environment), environment, board)
}

object DeterministicGridMdp {
    def apply(rows: Int = 2, cols: Int = 3, gamma: Double = 0.9) = {
        val q = Q.q[Tile, Move](gamma)
        val agent = Agent.qAgent(q, Agent.randomActionSelector(Up, Down, Left, Right))

        val board = Array.ofDim[Tile](rows, cols)
        for {
            r <- 0 until rows
            c <- 0 until cols
        } {
            board(r)(c) = new Tile(r, c)
        }

        val env = Environment[Tile, Move](reward(board), delta(board), board(0)(0), s => s == board(0)(board(0).length - 1))
        new DeterministicGridMdp(agent, env, board)
    }

    private def reward(board: Array[Array[Tile]])(t: Tile, m: Move, next: Tile) =
        if(t == board(0)(board(0).length - 1))
            0.0
        else {
            if(next == board(0)(board(0).length - 1))
                100.0
            else 0.0
        }

    private def delta(board: Array[Array[Tile]])(t: Tile, m: Move) =
        if(t == board(0)(board(0).length - 1))
            t
        else {
            val next = (t.row + m.transform()._1, t.col + m.transform()._2)
            if(next._1 < 0 || next._1 >= board.length)
                t
            else if(next._2 < 0 || next._2 >= board(0).length)
                t
            else board(next._1)(next._2)
        }
}
