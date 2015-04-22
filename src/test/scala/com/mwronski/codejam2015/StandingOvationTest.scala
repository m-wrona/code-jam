package com.mwronski.codejam2015

import com.mwronski.codejam2015.io.StandingOvationTask
import com.mwronski.test.TestUtils
import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

import scala.io.Source

/**
 * Test cases for standing ovation problem
 *
 * @see com.mwronski.codejam2015.StandingOvation
 * @see https://code.google.com/codejam/contest/6224486/dashboard
 * @author Michal Wronski
 */
class StandingOvationTest extends FunSpec with GivenWhenThen with Matchers with TestUtils
with StandingOvation {

  describe("2015-A: Standing ovation") {

    describe("Sample tasks") {

      it("Should check audience: 11111") {
        check(Array(1, 1, 1, 1, 1)) should be(0)
      }

      it("Should check audience: 09") {
        check(Array(0, 9)) should be(1)
      }

      it("Should check audience: 110011") {
        check(Array(1, 1, 0, 0, 1, 1)) should be(2)
      }

      it("Should check audience: 1") {
        check(Array(1)) should be(0)
      }

      it("Should check audience: 01") {
        check(Array(0, 1)) should be(1)
      }

      it("Should check audience: 4069616") {
        check(Array(4, 0, 6, 9, 6, 1, 6)) should be(0)
      }

      it("Should check audience: 0000001") {
        check(Array(0, 0, 0, 0, 0, 0, 1)) should be(6)
      }

      it("Should check audience: 5050001") {
        check(Array(5, 0, 5, 0, 0, 0, 1)) should be(0)
      }

    }

    describe("File tasks") {

      it("Should solve small tasks") {
        Given("small tasks")
        val in = StandingOvationTask(Source.fromFile("src/test/resources/2015/A-small-practice.in").getLines())
        And("solutions for small tasks ")
        val out = Source.fromFile("src/test/resources/2015/A-small-practice.out").getLines()

        When("solving small tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[Int]()
        for (line <- in) {
          line match {
            case Left(error) => fail("Parsing error: " + line)

            case Right(task) => solutions += check(task.audience)
          }
        }

        Then("all tasks are solved correctly")
        var nr = 0
        for (expected <- parseResults(out)) {
          solutions(nr) should be(expected)
          nr += 1
        }
      }

      it("Should solve large tasks") {
        Given("large tasks")
        val in = StandingOvationTask(Source.fromFile("src/test/resources/2015/A-large-practice.in").getLines())
        And("solutions for large tasks ")
        val out = Source.fromFile("src/test/resources/2015/A-large-practice.out").getLines()

        When("solving large tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[Int]()
        for (line <- in) {
          line match {
            case Left(error) => fail("Parsing error: " + line)

            case Right(task) => solutions += check(task.audience)
          }
        }

        Then("all tasks are solved correctly")
        var nr = 0
        for (expected <- parseResults(out)) {
          solutions(nr) should be(expected)
          nr += 1
        }
      }

    }

  }

}
