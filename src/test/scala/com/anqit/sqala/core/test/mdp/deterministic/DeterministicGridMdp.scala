package com.anqit.sqala.core.test.mdp.deterministic

import com.anqit.sqala.components.{Action, State}
import com.anqit.sqala.core.Learner
import com.anqit.sqala.mdp.{Agent, Environment, MDP, Q}

class DeterministicGridMdp private(agent: Agent[Tile, Move], environment: Environment[Tile, Move], board: Array[Array[Tile]]) extends MDP[Tile, Move](agent, environment) {
    val rows = board.length
    val cols = board(0).length

    override def learn() = new DeterministicGridMdp(Learner.learn(agent, environment), environment, board)

    def printQ: Unit = {
        System.out.println("== Q Values =========")
        val q = agent.q

        for {
            r <- 0 until rows
        } {
            for {
                c <- 0 until cols
            } {

                System.out.print("      " + q(board(r)(c), Up) + "       ")
            }
            System.out.println
            for {
                c <- 0 until cols
            } {
                System.out.print(q(board(r)(c), Left))
                System.out.print(" " + board(r)(c) + " ")
                System.out.print(q(board(r)(c), Right) + " ")
            }
            System.out.println
            for {
                c <- 0 until cols
            } {

                System.out.print("      " + q(board(r)(c), Down) + "       ")
            }
            System.out.println
        }
        System.out.println("=====================")
    }

    def printPolicy: Unit = {
        System.out.println("== Policy ===========")
        val policy = agent.policy

        for {
            r <- 0 until rows
        } {
            for {
                c <- 0 until cols
            } {
                System.out.print("      ")
                if(policy(board(r)(c)) == Up) {
                    System.out.print(Up)
                } else {
                    System.out.print(" ")
                }
                System.out.print("    ")
            }
            System.out.println

            for {
                c <- 0 until cols
            } {
                System.out.print(" ")
                if(policy(board(r)(c)) == Left) {
                    System.out.print(Left)
                } else {
                    System.out.print(" ")
                }
                System.out.print(" " + board(r)(c) + " ")
                if(policy(board(r)(c)) == Right) {
                    System.out.print(Right)
                } else {
                    System.out.print(" ")
                }
            }
            System.out.println

            for {
                c <- 0 until cols
            } {
                System.out.print("      ")
                if(policy(board(r)(c)) == Down) {
                    System.out.print(Down)
                } else {
                    System.out.print(" ")
                }
                System.out.print("    ")
            }
            System.out.println
        }
        System.out.println("=====================")
    }
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
            val next = delta(board)(t, m)
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

class Tile(val row: Int, val col: Int) extends State {
    override def toString: String = s"(${row}, ${col})"
}

abstract class Move extends Action {
    def transform(): (Int, Int)
}
object Up extends Move {
    override def transform(): (Int, Int) = (-1, 0)
    override def toString() = "▲"
}
object Down extends Move {
    override def transform(): (Int, Int) = (1, 0)
    override def toString() = "▼"
}
object Left extends Move {
    override def transform(): (Int, Int) = (0, -1)
    override def toString() = "◄"
}
object Right extends Move {
    override def transform(): (Int, Int) = (0, 1)
    override def toString() = "►"
}