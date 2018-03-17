package com.anqit.sqala.core.test.mdp.nondeterministic

import com.anqit.sqala.core.Learner
import com.anqit.sqala.core.test.mdp._
import com.anqit.sqala.mdp.{Agent, Environment, MDP, Q}

import scala.util.Random

/**
  * @see <a href="https://web.uvic.ca/~maryam/AISpring94/Slides/07_MDP.pdf">sauce</a>
  * @param agent
  * @param environment
  * @param board
  */
class NonDeterministicGridMdp private(agent: Agent[Tile, Move], environment: Environment[Tile, Move], board: Array[Array[Tile]]) extends GridMdp(agent, environment, board) {
    override def learn() = new NonDeterministicGridMdp(Learner.learn(agent, environment), environment, board)
}

object NonDeterministicGridMdp {
    def apply(rows: Int = 3, cols: Int = 4, gamma: Double = 0.9) = {
        val q = Q.q[Tile, Move](gamma)
        val agent = Agent.qAgent(q, Agent.randomActionSelector(Up, Down, Left, Right))

        val board = Array.ofDim[Tile](rows, cols)
        for {
            r <- 0 until rows
            c <- 0 until cols
        } {
            board(r)(c) = new Tile(r, c)
        }

        val env = Environment[Tile, Move](reward(board), delta(board), board(0)(0), isTerminalState(board))
        new NonDeterministicGridMdp(agent, env, board)
    }

    private def isTerminalState(board: Array[Array[Tile]])(s: Tile) =
        s == board(0)(board(0).length - 1) || s == board(1)(board(1).length - 1)

    private def reward(board: Array[Array[Tile]])(t: Tile, m: Move, next: Tile) =
        if(isTerminalState(board)(t))
            0.0
        else {
            if(next == board(0)(board(0).length - 1))
                1.0
            else if(next == board(1)(board(1).length - 1))
                -1.0
            else -0.03
        }

    private def delta(board: Array[Array[Tile]])(t: Tile, m: Move) =
        if(t == board(0)(board(0).length - 1))
            t
        else {
            val roll = Random.nextDouble()

            val transform = (roll match {
                case x if x < .1 => m.cw
                case y if y < .2 => m.ccw
                case _ => m
            }).transform()

            val next = (t.row + transform._1, t.col + transform._2)
            if(next._1 < 0 || next._1 >= board.length)
                t
            else if(next._2 < 0 || next._2 >= board(0).length)
                t
            else if(next._1 == 1 && next._2 == 1)
                t
            else board(next._1)(next._2)
        }
}
