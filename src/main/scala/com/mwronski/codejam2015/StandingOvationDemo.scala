package com.mwronski.codejam2015

import com.mwronski.codejam2015.io.StandingOvationTask

import scala.io.Source

/**
 * Runner for standing ovation task
 *
 * @author Michal Wronski
 */
object StandingOvationDemo extends StandingOvation {

  val files = List(
    "doc/2015/standing_ovation/A-small-practice.in",
    "doc/2015/standing_ovation/A-large-practice.in"
  )

  def main(args: Array[String]) {
    for (file <- files) {
      println("\n************************* %s ************************\n".format(file))
      demo(file)
    }
  }

  /**
   * Execute all tasks in given file
   * @param file file which tasks should be executed
   */
  private def demo(file: String) = {
    var nr = 1
    for (
      task <- StandingOvationTask(Source.fromFile(file).getLines())
    ) {
      task match {
        case Left(error) => println("Case #%d - error: %s".format(nr, error))

        case Right(task) => println("Case #%d: %d".format(nr, check(task.audience)))
      }
      nr += 1
    }
  }


}
