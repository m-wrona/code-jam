package com.mwronski.codejam2015.io

/**
 * Parser for standing ovation input data.
 *
 * The first line of the input gives the number of test cases, T.
 * T test cases follow. Each consists of one line with Smax, the maximum shyness level of the shyest person in the audience,
 * followed by a string of Smax + 1 single digits. The kth digit of this string (counting starting from 0) represents
 * how many people in the audience have shyness level k. For example, the string "409" would mean that there were
 * four audience members with Si = 0 and nine audience members with Si = 2 (and none with Si = 1 or any other value).
 * Note that there will initially always be between 0 and 9 people with each shyness level.
 *
 * The string will never end in a 0. Note that this implies that there will always be at least one person in the audience.
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard
 * @author Michal Wronski
 */
object StandingOvationTask {

  /**
   * Parse task data
   * @param data data with tasks
   * @return non-nullable iterator with parsing results
   */
  def apply(data: Iterator[String]): Iterator[Either[String, StandingOvationTask]] = {
    data.next() //skip first line
    data.map(
      row => {
        val data = row.split(" ")
        if (data.length != 2) {
          Left("Unexpected line format: %s".format(row))
        } else {
          try {
            val maxShyness = data(0).toInt
            val audience = data(1).toCharArray.map(_.toInt)
            val expectedAudience = audience.length - 1
            if (maxShyness != expectedAudience) {
              Left("Couldn't parse line '%s' - max shyness level '%d' different then audience length '%d'".format(row, maxShyness, expectedAudience))
            } else {
              Right(new StandingOvationTask(maxShyness, audience))
            }
          } catch {
            case t: Throwable => Left("Error while parsing line: %s, error: %s".format(row, t))
          }
        }
      }
    )
  }

}

/**
 * Single task related for standing ovation
 * @param maxShyness the maximum shyness level of the shyest person in the audience
 * @param audience people grouped by shyness level (index: shyness level, value: number of persons with given shyness)
 */
sealed case class StandingOvationTask(maxShyness: Int, audience: Array[Int])
