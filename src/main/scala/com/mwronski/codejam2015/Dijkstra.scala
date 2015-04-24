package com.mwronski.codejam2015

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
   * Quaternions multiplicative structure
   */
  private val quaternions = Map(
    "1" -> Map(
      "1" -> "1",
      "i" -> "i",
      "j" -> "j",
      "k" -> "k"
    ),
    "i" -> Map(
      "1" -> "i",
      "i" -> "-1",
      "j" -> "k",
      "k" -> "-j"
    ),
    "j" -> Map(
      "1" -> "j",
      "i" -> "-k",
      "j" -> "-1",
      "k" -> "i"
    )
    ,
    "k" -> Map(
      "1" -> "k",
      "i" -> "j",
      "j" -> "-i",
      "k" -> "-1"
    )
  )

  /**
   * i*j*k=-1
   */
  private val ijk = -1

  /**
   * Check whether given text can be reduced to ijk
   * @param text text to be reduced
   * @param repeated repeat text n-times (needed for extremely long text that's hard to keep in-memory)
   * @return check result
   */
  final def canReduce(text: Array[Char], repeated: Long = 1): Boolean = {
    //count whole result - as text is repeated up to 3 mult must be made
    val part = reduce(text, 0, text.length)
    var all = part
    for (i <- 1 until (repeated % 4).toInt) {
      all = multiply(all, part)
    }
    //check whole result and check indexes if needed
    val textLength: Long = text.length * repeated
    if (all == ijk) {
      return tryReduceTo('i', text, 0, textLength)
        .flatMap(
          Pi => {
            tryReduceTo('j', text, Pi + 1, textLength)
              .map(Pj => Pj < textLength)
          }
        )
        .getOrElse(false)
    } else {
      false
    }
  }

  /**
   * Try reduce text to expecting result and return end index when result is fulfilled
   * @param expected expected result
   * @param text whole text
   * @param from start index
   * @param to max end index
   * @return non-nullable option of found end index when expected result has been fulfilled
   */
  private def tryReduceTo(expected: Char, text: Array[Char], from: Long, to: Long): Option[Long] = {
    var result = 1
    var i: Long = from
    while (i < to) {
      result = reduce(text, i, i + 1, result)
      if (toQuaternion(expected) == result) {
        return Some(i)
      }
      i += 1
    }
    None
  }

  /**
   * Reduce given part of text according to quaternion
   * @param text whole text
   * @param from start index
   * @param to end index
   * @param init init value before reduce start
   * @return non-null results
   */
  private def reduce(text: Array[Char], from: Long, to: Long, init: Int = 1): Int = {
    var result = init
    var i: Long = from
    while (i < to) {
      val c = text((i % text.length).toInt)
      result = multiply(result, toQuaternion(c))
      i += 1
    }
    result
  }

}

/**
 * Quaternions and related rules
 */
sealed trait Quaternions {

  import scala.math.abs

  private val i = 2
  private val j = 3
  private val k = 4

  private val mult = Array(
    Array(0, 0, 0, 0, 0), //dummy for indexing
    Array(0, 1, i, j, k), //1
    Array(0, i, -1, k, -j), //i
    Array(0, j, -k, -1, i), //j
    Array(0, k, j, -i, -1) //k
  )

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

  /**
   * Multiply quaternions
   * @param x char representation of 1st quaternion
   * @param y char representation of 2nd quaternion
   * @return result
   */
  final def multiply(x: Char, y: Char): Int = multiply(toQuaternion(x), toQuaternion(y))

  /**
   * Convert char into quaternion according to multiplicative table
   * @param c char
   * @return quaternion value
   */
  final def toQuaternion(c: Char): Int = c.toInt - ('i'.toInt - 2)

}
