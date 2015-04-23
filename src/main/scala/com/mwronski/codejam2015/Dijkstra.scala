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
   * @return check result
   */
  final def canReduce(text: String): Boolean = {
    if (
      text.length >= 3
        && canReduceTo(ijk, text, 0, text.length)
    ) {
      return tryReduceTo("i", text, 0, text.length)
        .flatMap(
          Pi => {
            tryReduceTo("j", text, Pi, text.length)
              .map(Pj => Pj < text.length)
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
  private def tryReduceTo(expected: String, text: String, from: Int, to: Int): Option[Int] = {
    for (i <- from until to) {
      if (canReduceTo(expected, text, from, i)) {
        return Some(i)
      }
    }
    None
  }

  /**
   * Check whether part of text can be reduced to given char according to quaternion
   * @param expected expected result of reducing
   * @param text whole text
   * @param from start index
   * @param to end index
   * @return check result
   */
  private def canReduceTo(expected: String, text: String, from: Int, to: Int): Boolean = {
    var result = "1"
    var negative = false
    for (c <- text.substring(from, to).toCharArray) {
      result = quaternions.get(result).get.get(c.toString).get
      if (result(0) == '-') {
        result = result(1).toString
        negative ^= true
      }
    }
    if (negative) {
      result = "-" + result
    }
    expected.equals(result)
  }


}
