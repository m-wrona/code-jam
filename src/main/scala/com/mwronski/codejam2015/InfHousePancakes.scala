package com.mwronski.codejam2015

/**
 * At the Infinite House of Pancakes, there are only finitely many pancakes,
 * but there are infinitely many diners who would be willing to eat them!
 * When the restaurant opens for breakfast, among the infinitely many diners, exactly D have non-empty plates;
 * the ith of these has Pi pancakes on his or her plate. Everyone else has an empty plate.
 *
 * Normally, every minute, every diner with a non-empty plate will eat one pancake from his or her plate.
 * However, some minutes may be special. In a special minute, the head server asks for the diners' attention,
 * chooses a diner with a non-empty plate, and carefully lifts some number of pancakes off of that diner's plate
 * and moves those pancakes onto one other diner's (empty or non-empty) plate. No diners eat during a special minute,
 * because it would be rude.
 *
 * You are the head server on duty this morning, and it is your job to decide which minutes, if any,
 * will be special, and which pancakes will move where. That is, every minute, you can decide to either
 * do nothing and let the diners eat, or declare a special minute and interrupt the diners
 * to make a single movement of one or more pancakes, as described above.
 *
 * Breakfast ends when there are no more pancakes left to eat.
 * How quickly can you make that happen?
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard#s=p1
 * @author Michal Wronski
 */
trait InfHousePancakes {

  /**
   * Minimum number of pancakes below which split is not viable
   */
  private final val minSplit = 4

  /**
   * Check how quickly given persons can eat their pancakes
   * @param persons persons with pancakes on their plates
   * @return non-negative number of turns
   */
  final def turns(persons: Array[Int]): Int = {
    val pancakesCount = persons
      .groupBy(pancakes => pancakes)
      .mapValues(persons => persons.length)
    val max = pancakesCount.max._1
    if (max < minSplit) {
      //costs below split limit - min value is optimum
      max
    } else {
      //divide and conquer
      splitPancakes(pancakesCount, 2 to max)
    }
  }

  /**
   * Check how quickly pancakes can be eaten while split among persons
   * @param pancakesCount number of pancakes (key) grouped by persons (value) who have to eat them
   * @param limits split limits for which number of turns should be checked
   * @return non-negative number of turns
   */
  private def splitPancakes(pancakesCount: Map[Int, Int], limits: Iterable[Int]): Int =
    limits
      .map(
        limit => {
          limit + pancakesCount
            //check how quickly pancakes will be eaten using given limit
            .map(pancakes => ((pancakes._1 - 1) / limit) * pancakes._2)
            .sum
        }
      )
      .min

}
