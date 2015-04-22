package com.mwronski.codejam2015

import com.mwronski.test.TestUtils
import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

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

      it("Should check: kij") {
        canReduce("kij") should be(false)
      }

      it("Should check: jijijijijiji") {
        canReduce("jijijijijiji") should be(true)
      }

    }

  }

}
