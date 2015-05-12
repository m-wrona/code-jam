package com.mwronski.codejam2015

import com.mwronski.codejam2015.io.StandingOvationTask
import com.mwronski.test.TestUtils
import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

import scala.io.Source

/**
 * Unit tests for Ominous challenge
 *
 * @author Michal Wronski
 */
class OminousTest extends FunSpec with GivenWhenThen with Matchers with TestUtils
with Ominous {

  describe("2015-D: Ominous Omino") {

    describe("Sample tasks") {

      it("Should solve: 2 2 2") {
        whoWins(2, 2, 2) should be("GABRIEL")
      }

      it("Should solve: 2 1 3") {
        whoWins(2, 1, 3) should be("RICHARD")
      }

      it("Should solve: 4 4 1") {
        whoWins(4, 4, 1) should be("RICHARD")
      }

      it("Should solve: 3 2 3") {
        whoWins(3, 2, 3) should be("GABRIEL")
      }

      it("Should solve:  4 2 4") {
        whoWins(4, 2, 4) should be("RICHARD")
      }

    }

    describe("File tasks") {

      it("Should solve small tasks") {
        Given("small tasks")
        val in = Source.fromFile("src/test/resources/2015/D-small-practice.in").getLines()
        in.next() //skip line with number of test cases
        And("solutions for small tasks ")
        val out = Source.fromFile("src/test/resources/2015/D-small-practice.out").getLines()

        When("solving small tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[String]()
        while (in.hasNext) {
          val data = in.next().split(" ").map(_.toInt)
          solutions += whoWins(data(0), data(1), data(2))
        }

        Then("all tasks are solved correctly")
        var nr = 0
        for (expected <- out.map(_.split(":")(1).trim)) {
          solutions(nr) should be(expected)
          nr += 1
        }
      }

      it("Should solve large tasks") {
        Given("large tasks")
        val in = Source.fromFile("src/test/resources/2015/D-large-practice.in").getLines()
        in.next() //skip line with number of test cases
        And("solutions for large tasks ")
        val out = Source.fromFile("src/test/resources/2015/D-large-practice.out").getLines()

        When("solving large tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[String]()
        while (in.hasNext) {
          val data = in.next().split(" ").map(_.toInt)
          solutions += whoWins(data(0), data(1), data(2))
        }

        Then("all tasks are solved correctly")
        var nr = 0
        for (expected <- out.map(_.split(":")(1).trim)) {
          solutions(nr) should be(expected)
          nr += 1
        }
      }

    }

  }

}
