package com.anqit.sqala.components

import scala.util.Random

trait Action {

}

object Action {
    def randomSelector[S <:State, A <: Action](a: A*) = (_: S) => a(Random.nextInt(a.size))
}
