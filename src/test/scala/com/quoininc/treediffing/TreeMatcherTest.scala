package com.quoininc.treediffing

import org.scalatest.FunSpec

import scala.collection.immutable.Seq

import TreeMatcher.NodeWithMatching

class TreeMatcherSpec extends FunSpec {

  describe("TreeMatcher") {
    describe("match") {
      it("should fully match identical tree") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b")))

        val matches = t1 matchWith t1.copy()
        matches foreach { t => assert(t._1 == t._2)}
      }

      it("should fully match very similar tree") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "var")))

        val t2 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "val")))

        val matches = t1 matchWith t2
        assert(matches.size == 5)
      }

      it("should match inner node with two out of three matching leaves") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(4, "A2", "a2"),
            Node(3, "A1", "a1"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b")))

        val matches = root matchWith root.replaceNode(3, null)
        assert(matches contains ((2, 2)))
      }
    }
  }
}
