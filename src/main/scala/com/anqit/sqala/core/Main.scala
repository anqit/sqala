package com.anqit.sqala.core

import com.anqit.sqala.components._

object Main {
    val gamma = 0.9

    val rows = 3
    val cols = 2
    val board = Array.ofDim[Tile](rows, cols)
    for {
        r <- 0 until rows
        c <- 0 until cols
    } {
        board(r)(c) = new Tile(r, c)
    }

    val states: Set[Tile] = board.flatMap(a => a.toSet).toSet
    val actions = Set(Up, Down, Left, Right)
    val d = new D(board)
    val r = new R(board, d)
    val runner = new Runner[Tile, Move](states, actions, r, d, gamma)
    val q = runner.run()

    def main(args: Array[String]): Unit = {
        System.out.print(q.values)
    }

    def p(q: Q[Tile, Move]) = {
        q.values
    }
}

class Tile(val row: Int, val col: Int) extends State {
    override def toString: String = s"(${row}, ${col})"
}

class D(board: Array[Array[Tile]]) extends Delta[Tile, Move] {
    override def apply(t: Tile, m: Move): Tile = {
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
    }
}

class R(board: Array[Array[Tile]], d: D) extends Reward[Tile, Move] {
    val goalReward = 100
    override def apply(t: Tile, m: Move): Double = {
        if(t == board(board.length - 1)(board(0).length - 1))
            0
        else {
            val next = d(t, m)
            if(next == board(board.length - 1)(board(0).length - 1))
                goalReward
            else 0
        }
    }
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