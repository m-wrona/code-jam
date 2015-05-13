package com.mwronski.codejam2015.qualification

import scala.math._

/**
 * An N-omino is a two-dimensional shape formed by joining N unit cells fully along
 * their edges in some way. More formally, a 1-omino is a 1x1 unit square, and an
 * N-omino is an (N-1)omino with one or more of its edges joined to an adjacent 1x1 unit square.
 * For the purpose of this problem, we consider two N-ominoes to be the same
 * if one can be transformed into the other via reflection and/or rotation.
 *
 * Richard and Gabriel are going to play a game with the following rules, for some predetermined values of X, R, and C:
 *
 * 1. Richard will choose any one of the possible X-ominoes.
 * 2. Gabriel must use at least one copy of that X-omino, along with arbitrarily many copies of
 * any X-ominoes (which can include the one Richard chose), to completely fill in an R-by-C grid,
 * with no overlaps and no spillover. That is, every cell must be covered by exactly one of
 * the X cells making up an X-omino, and no X-omino can extend outside the grid.
 * Gabriel is allowed to rotate or reflect as many of the X-ominoes as he wants, including the one Richard chose.
 * If Gabriel can completely fill in the grid, he wins; otherwise, Richard wins.
 *
 * Given particular values X, R, and C, can Richard choose an X-omino that will ensure that he wins,
 * or is Gabriel guaranteed to win no matter what Richard chooses?
 *
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard#s=p3
 * @author Michal Wronski
 */
trait Ominous {

  private val richard = "RICHARD"
  private val gabriel = "GABRIEL"

  /**
   * Check who's gonna win the game
   * @param x X-ominoes
   * @param r number of rows in a grid
   * @param c number of columns in a grid
   * @return non-nullable name of winning person
   */
  final def whoWins(x: Int, r: Int, c: Int): String = {
    //check generic scenarios
    if (x >= 7) {
      //ominoes can contain the gap in the center that cannot by never filled
      return richard
    } else if ((r * c) % x != 0) {
      //no enough space
      return richard
    }
    //check detail scenarios
    val s = min(r, c)
    val l = max(r, c)
    if (x == 3 && s == 1) {
      return richard
    } else if (x == 4 && s <= 2) {
      return richard
    } else if (x == 5 && s <= 2) {
      return richard
    } else if (x == 5 && s == 3 && l == 5) {
      return richard
    } else if (x == 6 && s <= 3) {
      return richard
    }
    else {
      //no enough space
      gabriel
    }
  }

}
