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
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard#s=p2
 * @author Michal Wronski
 */
trait Dijkstra {

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
   * Check whether given text can be reduced to ijk
   * @param text text to be reduced
   * @return check result
   */
  final def canReduce(text: String): Boolean = {
    if (text.length < 3) {
      //text is too short to be reduced
      false
    }
    for (
      i <- 0 until text.length;
      j <- 1 until text.length if j > i;
      k <- 2 until text.length if k > i && k > j
    ) {
      if (
        canReduceTo('i', text, i, j)
          && canReduceTo('j', text, j, k)
          && canReduceTo('k', text, k, text.length)
      ) {
        return true
      }
    }
    false
  }

  /**
   * Check whether part of text can be reduced to given char according to quaternion
   * @param char char to which sub-text should be reduced
   * @param text whole text
   * @param from start index
   * @param to end index
   * @return check result
   */
  private def canReduceTo(char: Char, text: String, from: Int, to: Int): Boolean = {
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
    char.toString.equals(result)
  }


}
