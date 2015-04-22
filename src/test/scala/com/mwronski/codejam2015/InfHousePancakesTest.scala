package com.mwronski.codejam2015

import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

/**
 * Unit tests related with pancakes problem
 *
 * @see InfHousePancakes
 * @author Michal Wronski
 */
class InfHousePancakesTest extends FunSpec with GivenWhenThen with Matchers
with InfHousePancakes {

  describe("Sample Infinite House of Pancakes tasks") {

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

  }

}
