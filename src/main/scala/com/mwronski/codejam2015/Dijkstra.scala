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
trait Dijkstra {

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
  private val ijk = "-1"

  /**
   * Check whether given text can be reduced to ijk
   * @param text text to be reduced
   * @param repeated repeat text n-times (needed for extremely long text that's hard to keep in-memory)
   * @return check result
   */
  final def canReduce(text: Array[Char], repeated: Long = 1): Boolean = {
    val textLength: Long = text.length * repeated
    if (
      textLength >= 3
        && ijk.equals(reduce(text, 0, textLength))
    ) {
      return tryReduceTo("i", text, 0, textLength)
        .flatMap(
          Pi => {
            tryReduceTo("j", text, Pi + 1, textLength)
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
  private def tryReduceTo(expected: String, text: Array[Char], from: Long, to: Long): Option[Long] = {
    var result = "1"
    for (i <- from until to) {
      result = reduce(text, i, i + 1, result)
      if (expected.equals(result)) {
        return Some(i)
      }
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
  private def reduce(text: Array[Char], from: Long, to: Long, init: String = "1"): String = {
    //init
    var result = init
    var negative = false
    val checkNegative = () => {
      if (result(0) == '-') {
        result = result(1).toString
        negative ^= true
      }
    }
    //reduce sub-text
    checkNegative()
    var i: Long = from
    while (i < to) {
      val c = text((i % text.length).toInt)
      result = quaternions.get(result).get.get(c.toString).get
      checkNegative()
      i += 1
    }
    //return result
    if (negative) {
      "-" + result
    } else {
      result
    }
  }

}
