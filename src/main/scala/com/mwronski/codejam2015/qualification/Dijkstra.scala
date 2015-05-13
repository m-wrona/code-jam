package com.mwronski.codejam2015.qualification

/**
 * The Dutch computer scientist Edsger Dijkstra made many important contributions to the field,
 * including the shortest path finding algorithm that bears his name. This problem is not about that algorithm.
 *
 * You were marked down one point on an algorithms exam for misspelling "Dijkstra" --
 * between D and stra, you wrote some number of characters, each of which was either i, j, or k.
 * You are prepared to argue to get your point back using quaternions, an actual number system
 * (extended from complex numbers) with the following multiplicative structure:
 *
 *
 *
 * To multiply one quaternion by another, look at the row for the first quaternion and the column
 * for the second quaternion. For example, to multiply i by j, look in the row for i and the column for j to find
 * that the answer is k. To multiply j by i, look in the row for j and the column for i to find that the answer is -k.
 *
 * As you can see from the above examples, the quaternions are not commutative -- that is, there are some
 * a and b for which a * b != b * a. However they are associative --
 * for any a, b, and c, it's true that a * (b * c) = (a * b) * c.
 *
 * Negative signs before quaternions work as they normally do -- for any quaternions a and b,
 * it's true that -a * -b = a * b, and -a * b = a * -b = -(a * b).
 *
 * You want to argue that your misspelling was equivalent to the correct spelling ijk by showing
 * that you can split your string of is, js, and ks in two places, forming three substrings, such that
 * the leftmost substring reduces (under quaternion multiplication) to i, the middle substring reduces to j,
 * and the right substring reduces to k. (For example, jij would be interpreted as j * i * j; j * i is -k,
 * and -k * j is i, so jij reduces to i.) If this is possible, you will get your point back.
 * Can you find a way to do it?
 *
 * Solution note:
 * 1. i*j*k = -1
 * 2. j*i*k = 1
 * 3. k*i*j = -1
 * 4. k*j*i = 1
 * 5. i*k*j = 1
 * 6. j*k*i = -1
 * Thus the best is to check whether whole string gives -1.
 * If yes only positions of i,j,k must be checked: Pi<Pj<Pk
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard#s=p2
 * @author Michal Wronski
 */
trait Dijkstra extends Quaternions {

  /**
   * i*j*k=-1
   */
  private val ijk = -1

  /**
   * Check whether given text can be reduced to ijk
   * @param text valid text to be reduced
   * @param repeated repeat text n-times (needed for extremely long text that's hard to keep in-memory)
   * @return check result
   */
  final def canReduce(text: Array[Char], repeated: Long = 1): Boolean = {
    //count whole result - as text is repeated up to 4 repetitions must be made
    val limit: Int = 4
    val part = text.foldLeft(1)((v: Int, c: Char) => multiply(v, Quaternions(c)))
    val all = (0 until (repeated % limit).toInt).foldLeft(1)((total, i) => multiply(total, part))
    //check whole result and check indexes if needed
    if (all == ijk) {
      return matchFwd('i', text, limit)
        .flatMap(
          Pi => {
            matchBwd('k', text, limit)
              .map(
                endPk => {
                  val Pk = (text.length * repeated) - endPk
                  Pi < Pk
                }
              )
          }
        )
        .getOrElse(false)
    } else {
      false
    }
  }

  /**
   * Try to match forward text to expected result
   * @param expected expected quaternion
   * @param text text to be matched
   * @param times number text repetition that can be checked
   * @return non-nullable option of found start index
   */
  private def matchFwd(expected: Char, text: Array[Char], times: Int): Option[Int] = {
    val out = Quaternions(expected)
    var result = 1
    for (n <- 0 until times) {
      for (i <- 0 until text.length) {
        val c = text(i)
        result = multiply(result, Quaternions(c))
        if (out == result) {
          return Some((n * text.length) + i)
        }
      }
    }
    None
  }

  /**
   * Try to match backward text to expected result
   * @param expected expected quaternion
   * @param text text to be matched
   * @param times number text repetition that can be checked
   * @return non-nullable option of found index counting from the end
   */
  private def matchBwd(expected: Char, text: Array[Char], times: Int = 1): Option[Int] = {
    val out = Quaternions(expected)
    var result = 1
    for (n <- 0 until times) {
      for (i <- 1 to text.length) {
        val c = text(text.length - i)
        result = multiply(Quaternions(c), result)
        if (out == result) {
          return Some((n * text.length) + i)
        }
      }
    }
    None
  }

}

/**
 * Quaternions and related rules
 */
object Quaternions {

  val i = apply('i')
  val j = apply('j')
  val k = apply('k')

  private val mult = Array(
    Array(0, 0, 0, 0, 0), //dummy for indexing
    Array(0, 1, i, j, k), //1
    Array(0, i, -1, k, -j), //i
    Array(0, j, -k, -1, i), //j
    Array(0, k, j, -i, -1) //k
  )

  /**
   * Convert char into quaternion according to multiplicative table
   * @param c char representation of quaternion
   * @return quaternion value
   */
  final def apply(c: Char): Int = c.toInt - ('i'.toInt - 2)

}

sealed trait Quaternions {

  import com.mwronski.codejam2015.qualification.Quaternions._

import scala.math.abs

  /**
   * Multiply quaternions
   * @param x 1st quaternion
   * @param y  2nd quaternion
   * @return result
   */
  final def multiply(x: Int, y: Int): Int = {
    val result = mult(abs(x))(abs(y))
    if (x * y > 0) {
      result
    } else {
      -result
    }
  }

}
