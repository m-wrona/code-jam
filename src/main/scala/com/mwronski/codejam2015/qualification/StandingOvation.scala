package com.mwronski.codejam2015.qualification

/**
 * It's opening night at the opera, and your friend is the prima donna (the lead female singer).
 * You will not be in the audience, but you want to make sure she receives a standing ovation --
 * with every audience member standing up and clapping their hands for her.
 *
 * Initially, the entire audience is seated. Everyone in the audience has a shyness level.
 * An audience member with shyness level Si will wait until at least Si other audience members have already stood up to clap,
 * and if so, she will immediately stand up and clap. If Si = 0, then the audience member will always stand up and clap immediately,
 * regardless of what anyone else does. For example, an audience member with Si = 2 will be seated at the beginning,
 * but will stand up to clap later after she sees at least two other people standing and clapping.
 *
 * You know the shyness level of everyone in the audience, and you are prepared to invite additional friends of the prima donna
 * to be in the audience to ensure that everyone in the crowd stands up and claps in the end. Each of these friends may have
 * any shyness value that you wish, not necessarily the same.
 * What is the minimum number of friends that you need to invite to guarantee a standing ovation?
 *
 *
 * @see https://code.google.com/codejam/contest/6224486/dashboard#s=p0
 * @author Michal Wronski
 */
trait StandingOvation {

  /**
   * Check how many friends are missing to guarantee a standing ovation
   * @param audience non-nullable audience according to shyness level
   * @return non-negative number
   */
  final def check(audience: Array[Int]): Int = {
    var peopleUp = audience(0)
    var totalMissing = 0
    for (shyness <- 1 until audience.length if audience(shyness) != 0) {
      if (peopleUp < shyness) {
        val missing = (shyness - peopleUp)
        totalMissing += missing
        peopleUp += missing //assume they got up and clap
      }
      peopleUp += audience(shyness)
    }
    totalMissing
  }

}
