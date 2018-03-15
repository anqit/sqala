package com.anqit.sqala.core.test.mdp

import com.anqit.sqala.mdp.{Agent, Environment, MDP}

class GridMdp protected(agent: Agent[Tile, Move], environment: Environment[Tile, Move], board: Array[Array[Tile]]) extends MDP[Tile, Move](agent, environment) {
    val rows = board.length
    val cols = board(0).length

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
