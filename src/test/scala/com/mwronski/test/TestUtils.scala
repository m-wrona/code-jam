package com.mwronski.test

/**
 * Utils related with testing
 *
 * @author Michal Wronski
 */
trait TestUtils {

  /**
   * Parse results of problems
   * @param lines result test cases
   * @return results of test cases
   */
  final def parseResults(lines: Iterator[String]) = lines.map(_.split(":")(1).trim.toInt)

}
