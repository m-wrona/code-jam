package com.mwronski.codejam2015

import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

/**
 * Test cases for standing ovation problem
 *
 * @see com.mwronski.codejam2015.StandingOvation
 * @see https://code.google.com/codejam/contest/6224486/dashboard
 * @author Michal Wronski
 */
class StandingOvationTest extends FunSpec with GivenWhenThen with Matchers
with StandingOvation {

  describe("Sample standing ovation tasks") {

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

}
