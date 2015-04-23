package com.mwronski.codejam2015

import com.mwronski.test.TestUtils
import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

import scala.io.Source

/**
 * Unit tests for Dijkstra problem
 *
 * @author Michal Wronski
 */
class DijkstraTest extends FunSpec with GivenWhenThen with Matchers with TestUtils
with Dijkstra {

  describe("2015-C: Dijkstra") {

    describe("Sample tasks") {

      it("Should check: i") {
        canReduce("i") should be(false)
      }

      it("Should check: ij") {
        canReduce("ij") should be(false)
      }

      it("Should check: ijk") {
        canReduce("ijk") should be(true)
      }

      it("Should check: jik") {
        canReduce("jik") should be(false)
      }

      it("Should check: kij") {
        canReduce("kij") should be(false)
      }

      it("Should check: kji") {
        canReduce("kji") should be(false)
      }

      it("Should check: ikj") {
        canReduce("ikj") should be(false)
      }

      it("Should check: jki") {
        canReduce("jki") should be(false)
      }

      it("Should check: jijijijijiji") {
        canReduce("jijijijijiji") should be(true)
      }

      it("Should check: kkkkkk") {
        canReduce("kkkkkk") should be(false)
      }

    }

    describe("File tasks") {

      it("Should solve small tasks") {
        Given("small tasks")
        val in = Source.fromFile("src/test/resources/2015/C-small-practice.in").getLines()
        in.next() //skip number of test cases
        And("solutions for small tasks ")
        val out = Source.fromFile("src/test/resources/2015/C-small-practice.out").getLines()

        When("solving small tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[String]()
        while (in.hasNext) {
          val lx = in.next().split(" ")
          val part = in.next()
          var string = ""
          for (i <- 0 until lx(1).toInt) {
            string += part
          }
          solutions += (if (canReduce(string)) "YES" else "NO")
        }

        Then("all tasks are solved correctly")
        var nr = 0
        for (expected <- out.map(_.split(":")(1).trim)) {
          solutions(nr) should be(expected)
          nr += 1
        }
      }
    }

    //TODO: walking through strings must be optimized in order to enable large tasks
//    it("Should solve large tasks") {
//      Given("large tasks")
//      val in = Source.fromFile("src/test/resources/2015/C-large-practice.in").getLines()
//      in.next() //skip number of test cases
//      And("solutions for large tasks ")
//      val out = Source.fromFile("src/test/resources/2015/C-large-practice.out").getLines()
//
//      When("solving large tasks")
//      val solutions = scala.collection.mutable.ArrayBuffer[String]()
//      while (in.hasNext) {
//        val lx = in.next().split(" ")
//        val part = in.next()
//        var string = ""
//        var i: Long = 0
//        while (i < lx(1).toLong) {
//          string += part
//          i = 1
//        }
//        solutions += (if (canReduce(string)) "YES" else "NO")
//      }
//
//      Then("all tasks are solved correctly")
//      var nr = 0
//      for (expected <- out.map(_.split(":")(1).trim)) {
//        solutions(nr) should be(expected)
//        nr += 1
//      }
//    }

  }

}
