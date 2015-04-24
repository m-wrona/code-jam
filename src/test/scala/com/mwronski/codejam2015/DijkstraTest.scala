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
        canReduce("i".toCharArray) should be(false)
      }

      it("Should check: ij") {
        canReduce("ij".toCharArray) should be(false)
      }

      it("Should check: ijk") {
        canReduce("ijk".toCharArray) should be(true)
      }

      it("Should check: jik") {
        canReduce("jik".toCharArray) should be(false)
      }

      it("Should check: kij") {
        canReduce("kij".toCharArray) should be(false)
      }

      it("Should check: kji") {
        canReduce("kji".toCharArray) should be(false)
      }

      it("Should check: ikj") {
        canReduce("ikj".toCharArray) should be(false)
      }

      it("Should check: jki") {
        canReduce("jki".toCharArray) should be(false)
      }

      it("Should check: ji x 6") {
        canReduce("ji".toCharArray, 6) should be(true)
      }

      it("Should check: jijijijijiji") {
        canReduce("jijijijijiji".toCharArray) should be(true)
      }

      it("Should check: kkkkkk") {
        canReduce("kkkkkk".toCharArray) should be(false)
      }

      it("Should check: i x 100") {
        canReduce("i".toCharArray, 100) should be(false)
      }

      it("Should check long string") {
        canReduce("jjiikikkjkkikjkikijiiikjjkiijiijikiikiikkikkjijkkjkiiijikkikkijikkjjkkkijkjiiiiikkikiiiikjikkjjjkijikjjikkjjiijkjkjkjjikijkjkijjjkiikkkkkkkijikijkjiijjkjjijikkjjijjijjjkiiikjkiijiikkjikkjijikijikijjkjkkkjkkiiiijkjikijkkjjkkjkjkiijiiiikijkkkjijkjjikkkjkkijiikikjjkiiijjijkikkiikijijikkikikjkkkjjjjkjjiiiikijiikijijjkiikjkjkiikjjkkjkjkkijiikiikjiiikiikkjkjiiijjjjkkiijjikkiijjjjkkkkkjikikijiijiiikkjikijjjjjjjkjjkkiijjjijjijkiijkjiikjijiiiiijkjjiikijkiikiikijikijikjiijijkiiiijkkiikkkkkiijjiijikkjkkjjjiikkiijjkkikjkkiiiiiiikjjikijkjjkikjjjjijjkkkiijjkikiijjkjiijikjjjijiijkkkkikiikjjijkkikijjjiijkikiiijjjkjikkjiikjkjjiiijjijijiijjiijjjjjkkikjkikjjkiikijiikjijjijjkjjikikkjkijjijjkikkkkiijkkkikikjjikijjijjjkjkkkjiikkikjkjijjikkkjjjikjkjkkkijikiikkkkjkijiijiijkkikikkkjijjiiiiijiiikjiikikjjkjijikikkkjjkjkijiikjjjiijjiikkkkjiiijkkikkijkjjjiikijiikjikjikkijkkikjiiijjjjijkkkkijjiiiikikjjikkijkikijiiikijkikiikkikkikjiiiijkjikkkikkjiikijkikiijkjjjjkijjiiiikikijkkiijijijkijkjiiijkjiikkjiijkkkiikkjkiijjijikjkijkjjkjikjkkjjjijijjkijjikjijjijijkjikijikjkijkkjjikkkiiiikikjjjiiiikkkkjjiikjijkjijkkkkkjkjkiikkiiiiiikjjkijkjkkkjkjikjkiikkjikkkjjkjjkkkkiiijjjikkikjkikjijiijjiijkjkkkjikkjjjkjiiijkkjiikiijjjkkjiijiikiiiikkkijiijkjkiijkiikjikkjkjjiiiiikjijkijjkkiijjjikjkikkjikkjiiiikjkjjiiikijkikijkkijjijkkjikkijkkjjjjkjkkkkkikiijkjikikkiikjikijkikjjkiijkkkkijkkiiiijjjkiijkkkikikikiikjkijjjiiijkkikkkiiikkkikjjjjijiiijijjjjikkkiijkikijkiijkjiikkijkkikiiijkijjjkijkkiikkjiikkkjkiikkikjjikjikjikkjkijjkjkkiiiikiijjjikiikikikijijkkjikkkjjikijkikkjjijijkikkiikjikikkikiiijkjjjjjkikjikiijjkiijjiijijjkjikkkikjkkikjjjiikiiijkiiikkiiikjjjijkijikkiiikjiikkkjikjkikiijiikikkkijjijkkjijjkiikjkjkjjjjkjkikjiijikkijjijiiikkkkjkijkkjkkiiijkkjiijkjkiiijiikjjiikkjiiiiikiijkikjjjkjkiiijikiiiijiijjikkjkjiijiiiijjjikjjkjikkijijjikkikkikikjkjjjikkkkkkjjjjkkiijkjkijkjiikkkiijijjiikijijiijjjikijikjkkkiiijjikjkkiijjjjijiiijkkiiiiijjjkjkiiijkikikjjikijikikikkjkjijjkjikjkjkkiikiiiijiikkkikikkjiijikkiikjjijkiijkkkkkijkijjkijjjiikjjijkkjjkijjijkkjikkikiijjkiijkkkijkijiijkkjkkjkkjjkjkjkjikjjijkijijjjjiikiikiijkikjkjkikjijjkijijkiijkikikkjkjjkjkijkikkjikkkkijjkkiikjiiiijkijjjiijjjkjkkijiikijijijjjikiijijiikkjjijjikikiijjijjjjjjkjiikjjkjkjkjikkkijjijjkiiijjjjijjijiiiikjjjkikiijijikijjjijikkijjjjjkkjjjkkiijjkkjkkjjjjjkjikkijjkiikjikiikiijiikjijiiiiiikikkijkjkijjijjijijiikikjkjjkkkikijkkjiiikjkjkjkkikjkkikikiijijkjkijjkijikiikikjjjkjkkjjkjkijiiiikjjiiijijiijijjijjkkijjjkkjkkijkijjiikjkkkjijkkjjjkkikijkikkkkkkkikkiiiiikikjkkjiijkkiikijkiiiikjiikjjkjkijjijjkijjjkjiijkijkjikjkkkkkkkjjjjkkikkjiikjkkkjkjjikijijijkjiijijjiijkijiikiikiijjiikiikkijkkijikjkiikiijjkkjjkijkkkkijjjkijijjkjkkjijjikkikjkiiikjkikkjjkjjjkkkkikkkkiikkiiikkkkkiiijjjikjikkkijiijjjjikkkkkkkjikkjiiijjkjiikkkkiikjjjjjjjkjikkjkkkkkikkjkkjjkkijkkikjjjkiikjijkkjkkkkkkkjkijjjkkkkjkkjiijjikikkkiikjkiijkkijikjkijjiiiikjjkkkjijiikiikkjijjjkiikjjkijijijkkjkkkiikkiijkkjijkjkkkjiikkiijjikikkijkijkkkijikjikjkjjkkjjjijjiiiikjkikjjkiijkjiikjjiikjkijiijkijjjkkjkjjjikikijkjjikkjijjikkkjjikjkjjkijkjiiiikjjijjkjjkjjjkjkjijiiikjkkkjikiijjjkjjkkjjjkkjjiiiiikjijkjijkijkiiiikikjkiiijkiijkkkkkkjiiikikjijijijjjjikkkjikikjiijjkkkijkkjjijjjjijkijiikkkijkkikikjiiikkijijkjkikjkikjikkiikiiijijiikijjiiiiiiiikiijkijjikiiikkjiijkjkjjjikjiijijkkjijjjjjiikjkiikiiikjjjkkjjjkijiiijiiikikiiijijikkijkkkkkkkjjkkikiiijijiijijijjjjkkjkjijkkiikkiikjijiiiiijkjkijjjikkjkjjiiijkkijkikikkkkiikiiikkijjkjkikkjkijkkkikjkjjiijijikijkjjikjkjkkkkkijkikkkkijkjkkkkkkijikkkkkijiiijjikkkjjiikjiikjijjijkjkiijiijiiikkjikjijjkkijkkjkijjkiiikjkjkiijkjkikjkkiki".toCharArray, 2) should be(true)
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
          val text = in.next().toCharArray
          val x = lx(1).toInt
          solutions += (if (canReduce(text, x)) "YES" else "NO")
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
        val in = Source.fromFile("src/test/resources/2015/C-large-practice.in").getLines()
        in.next() //skip number of test cases
        And("solutions for large tasks ")
        val out = Source.fromFile("src/test/resources/2015/C-large-practice.out").getLines()

        When("solving large tasks")
        val solutions = scala.collection.mutable.ArrayBuffer[String]()
        while (in.hasNext) {
          val lx = in.next().split(" ")
          val text = in.next().toCharArray
          val x = lx(1).toLong
          solutions += (if (canReduce(text, x)) "YES" else "NO")
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
