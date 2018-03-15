package com.anqit.sqala.core.test.mdp

import com.anqit.sqala.components.State

class Tile(val row: Int, val col: Int) extends State {
    override def toString: String = s"(${row}, ${col})"
}
