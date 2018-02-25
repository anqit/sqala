package com.anqit.sqala.core.test

import com.anqit.sqala.components.{Action, State}
import com.anqit.sqala.core.Learner
import com.anqit.sqala.mdp.{Agent, Environment, Q}
import org.scalatest.{BeforeAndAfter, FunSuite}

class LearnerTest extends FunSuite with BeforeAndAfter {
    val gamma = 0.9

    val rows = 3
    val cols = 2
    var board: Array[Array[Tile]] = _

    before{
        board = Array.ofDim[Tile](rows, cols)
        for {
            r <- 0 until rows
            c <- 0 until cols
        } {
            board(r)(c) = new Tile(r, c)
        }
    }

    test("basic") {
        val q = Q.q[Tile, Move](.9)
        val env = Environment.env[Tile, Move](reward, delta, board(0)(0), s => s == board(board.length - 1)(board(0).length - 1))

        val agent = Learner.learn(Agent.qAgent(q, Action.randomSelector(Up, Down, Left, Right)), env)
        System.out.print(agent.q)
    }

    class Tile(val row: Int, val col: Int) extends State {
        override def toString: String = s"(${ row }, ${ col })"
    }

    def reward(t: Tile, m: Move): Double =
        if(t == board(board.length - 1)(board(0).length - 1))
            0.0
        else {
            val next = delta(t, m)
            if(next == board(board.length - 1)(board(0).length - 1))
                100.0
            else 0.0
        }

    def delta(t: Tile, m: Move) =
        if(t == board(board.length - 1)(board(0).length - 1))
            t
        else {
            val next = (t.row + m.transform()._1, t.col + m.transform()._2)
            if(next._1 < 0 || next._1 >= board.length)
                t
            else if(next._2 < 0 || next._2 >= board(0).length)
                t
            else board(next._1)(next._2)
        }

    abstract class Move extends Action {
        def transform(): (Int, Int)
    }
    object Up extends Move {
        override def transform(): (Int, Int) = (-1, 0)
        override def toString() = "^"
    }
    object Down extends Move {
        override def transform(): (Int, Int) = (1, 0)
        override def toString() = "\\/"
    }
    object Left extends Move {
        override def transform(): (Int, Int) = (0, -1)
        override def toString() = "<"
    }
    object Right extends Move {
        override def transform(): (Int, Int) = (0, 1)
        override def toString() = ">"
    }
}
