package com.mwronski.codejam2015.io

import org.scalatest.{Matchers, GivenWhenThen, FunSpec}

/**
 * Test cases for parsing data related with tasks
 *
 * @author Michal Wronski
 */
class StandingOvationTaskTest extends FunSpec with GivenWhenThen with Matchers {

  describe("Parsing of standing ovation tasks") {

    it("Should parse correct data") {
      Given("correct data")
      val data = List(
        "3",
        "6 4069616",
        "0 1",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)
      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 0, 6, 9, 6, 1, 6))

      tasks(1).right.getOrElse(null) should not be (null)
      tasks(1).right.get.maxShyness should be(0)
      tasks(1).right.get.audience should be(Array(1))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))
    }

    it("Should parse data with wrong number of test cases") {
      Given("sample data")
      And("wrong numer of test cases is given in the header")
      val data = List(
        "666",
        "6 4069616",
        "0 1",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)
      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 0, 6, 9, 6, 1, 6))

      tasks(1).right.getOrElse(null) should not be (null)
      tasks(1).right.get.maxShyness should be(0)
      tasks(1).right.get.audience should be(Array(1))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))
    }

    it("Should parse data with incorrect rows") {
      Given("sample data")
      And("2nd row has incorrect format")
      val data = List(
        "666",
        "6 4069616",
        "0 1 2 3 4",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)

      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 0, 6, 9, 6, 1, 6))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))

      And("proper parsing errors are returned")
      tasks(1).right.getOrElse(null) should be(null)
      tasks(1).left.get should be("Unexpected line format: 0 1 2 3 4")
    }


    it("Should parse data where audience is shorter than given shyness level") {
      Given("sample data")
      And("2nd row has smaller audience than set shyness level")
      val data = List(
        "666",
        "6 4069616",
        "2 1",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)
      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 0, 6, 9, 6, 1, 6))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))

      And("proper parsing errors are returned")
      tasks(1).right.getOrElse(null) should be(null)
      tasks(1).left.get should be("Couldn't parse line '2 1' - max shyness level '2' different then audience length '0'")
    }

    it("Should parse data with rows with non-integers values") {
      Given("sample data")
      And("1st row has non-integer values")
      val data = List(
        "3",
        "6 4x69616",
        "0 1",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)
      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 33, 6, 9, 6, 1, 6))

      tasks(1).right.getOrElse(null) should not be (null)
      tasks(1).right.get.maxShyness should be(0)
      tasks(1).right.get.audience should be(Array(1))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))
    }

    it("Should parse data with too few columns") {
      Given("sample data")
      And("2nd row has only one column")
      val data = List(
        "3",
        "6 4069616",
        "01",
        "1 01"
      )

      When("parsing data")
      val tasks = StandingOvationTask(data.iterator).toList

      Then("data is parsed without errors")
      tasks.length should be(3)
      And("proper tasks are created")
      tasks(0).right.getOrElse(null) should not be (null)
      tasks(0).right.get.maxShyness should be(6)
      tasks(0).right.get.audience should be(Array(4, 0, 6, 9, 6, 1, 6))

      tasks(2).right.getOrElse(null) should not be (null)
      tasks(2).right.get.maxShyness should be(1)
      tasks(2).right.get.audience should be(Array(0, 1))

      And("proper parsing errors are returned")
      tasks(1).right.getOrElse(null) should be(null)
      tasks(1).left.get should be("Unexpected line format: 01")
    }

  }

}
