package com.anqit.sqala.core.test.mdp

import com.anqit.sqala.components.Action

abstract class Move extends Action {
    def transform(): (Int, Int)
    def cw: Move
    def ccw: Move
}
object Up extends Move {
    override def transform(): (Int, Int) = (-1, 0)
    override def cw = Right
    override def ccw = Left

    override def toString() = "▲"
}
object Down extends Move {
    override def transform(): (Int, Int) = (1, 0)
    override def cw = Left
    override def ccw = Right

    override def toString() = "▼"
}
object Left extends Move {
    override def transform(): (Int, Int) = (0, -1)
    override def cw = Up
    override def ccw = Down

    override def toString() = "◄"
}
object Right extends Move {
    override def transform(): (Int, Int) = (0, 1)
    override def cw = Down
    override def ccw = Up

    override def toString() = "►"
}