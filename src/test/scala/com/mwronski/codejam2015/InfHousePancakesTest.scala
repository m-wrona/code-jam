package com.mwronski.codejam2015

import com.mwronski.test.TestUtils
import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

import scala.io.Source

/**
 * Unit tests related with pancakes problem
 *
 * @see InfHousePancakes
 * @author Michal Wronski
 */
class InfHousePancakesTest extends FunSpec with GivenWhenThen with Matchers with TestUtils
with InfHousePancakes {

  describe("2015-B: Infinite House of Pancakes") {

    describe("Sample tasks") {

      it("Should solve issue: 3") {
        turns(Array(3)) should be(3)
      }

      it("Should solve issue: 1 2 1 2") {
        turns(Array(1, 2, 1, 2)) should be(2)
      }

      it("Should solve issue: 4") {
        turns(Array(4)) should be(3)
      }

      it("Should solve issue: 4 8 7 8 3") {
        turns(Array(4, 8, 7, 8, 3)) should be(7)
      }

      it("Should solve issue: 6 6") {
        turns(Array(6, 6)) should be(5)
      }

      it("Should solve issue: 3 2 6") {
        turns(Array(3, 2, 6)) should be(4)
      }

    }

    describe("File tasks") {

      it("Should solve small tasks") {
        Given("small tasks")
        val in = Source.fromFile("src/test/resources/2015/B-small-practice.in").getLines()
        in.next() //skip number of test cases
        And("solutions for small tasks ")
        val out = Source.fromFile("src/test/resources/2015/B-small-practice.out").getLines()

        When("solving small tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[Int]()
        while (in.hasNext) {
          val numberOfPeople = in.next().toInt
          val pancakes = in.next().split(" ").map(_.toInt)
          solutions += turns(pancakes)
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
        val in = Source.fromFile("src/test/resources/2015/B-large-practice.in").getLines()
        in.next() //skip number of test cases
        And("solutions for large tasks ")
        val out = Source.fromFile("src/test/resources/2015/B-large-practice.out").getLines()

        When("solving large tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[Int]()
        while (in.hasNext) {
          val numberOfPeople = in.next().toInt
          val pancakes = in.next().split(" ").map(_.toInt)
          solutions += turns(pancakes)
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
