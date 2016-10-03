package com.quoininc.treediffing

import org.scalatest._

import scala.collection.immutable.{Set, Seq}
import Node._

class EditScriptSpec extends FunSpec with Matchers {

  describe("EditScript") {
    describe("lcs") {
      def toIds(lcs: Seq[(Node, Node)]) = lcs.map { case (a, b) => (a.id, b.id) }

      it("should return correctly for empty inputs") {
        val matchSet = Set[(NodeId, NodeId)]()
        val lcs = EditScript.lcs(Seq[Node](), Seq[Node](), matchSet)
        lcs shouldBe Seq[(Node, Node)]()
      }

      it("should return correctly matched 1 length node Seqs") {
        val matchSet = Set[(Node, Node)]()
        val n1 = Node(1, "A", "a")
        val n2 = Node(2, "B", "b")

        val lcs = EditScript.lcs(
          Seq(n1),
          Seq(n2),
          Set((n1.id, n2.id)))

        lcs shouldBe Seq((n1, n2))
      }

      it("should return correctly for 2-length seqs with common suffix") {
        val n1 = Node(1, "A", "a")
        val n2 = Node(2, "B", "b")

        val lcs = EditScript.lcs(
          Seq(Node(3, "C", "c"), n1),
          Seq(Node(4, "D", "d"), n2),
          Set((n1.id, n2.id)))

        lcs shouldBe Seq((n1, n2))
      }

      it("should return correctly for complex seqs") {
        val matchSeq = Seq((1, 2), (3, 4))

        val lcs = EditScript.lcs(
          Seq(Node(1, "A", "a"), Node(5, "E", "e"), Node(3, "C", "c")),
          Seq(Node(6, "F", "f"), Node(2, "B", "b"), Node(4, "D", "d")),
          matchSeq.toSet)

        toIds(lcs) shouldBe matchSeq
      }
    }

    describe("generate") {
      it("should return empty edit ops list for identical trees") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"))),
          Node(4, "B", "b")))

        val ops = EditScript.generate(root, root)
        ops shouldBe Seq()
      }

      it("should only give update for trees with only differing value on one node") {
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

        val ops = EditScript.generate(t1, t2)
        ops should contain theSameElementsInOrderAs Seq(Update(5, "val"))
      }

      it("should return one delete op for removed leaf") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(4, "A2", "a2"),
            Node(3, "A1", "a1"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b")))

        val ops = EditScript.generate(root, root.replaceNode(3, null))
        ops shouldBe Seq(Delete(3))
      }

      it("should return one insert op for inserted leaf") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b")))

        val insert = Insert(Node(7, "A4", "a4"), 2, 1)
        val ops = EditScript.generate(t1, insert apply t1)
        ops shouldBe Seq(insert)
      }

      it("should return move for reordered leaf node") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b")))

        val move = Move(5, 2, 1)
        val ops = EditScript.generate(t1, move apply t1)
        ops shouldBe Seq(move)
      }

      it("should return move for reordered inner node") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b", Seq(
            Node(7, "B1", "b1")))))

        val move = Move(6, 1, 0)
        val ops = EditScript.generate(t1, move apply t1)
        ops shouldBe Seq(move)
      }

      it("should return move for leaf transferred to new parent") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"),
            Node(5, "A3", "a3"))),
          Node(6, "B", "b", Seq(
            Node(7, "B1", "b1")))))

        val move = Move(3, 6, 1)
        val ops = EditScript.generate(t1, move apply t1)
        ops shouldBe Seq(move)
      }

      it("should return all types of ops for complex trees") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2", Seq(
              Node(5, "A2a", "a2a"))),
            Node(6, "A3", "a3"))),
          Node(7, "B", "b", Seq(
            Node(8, "B1", "b1")))))

        val t2 = Node(1, "ROOT", "", Seq(
          Node(9, "C", "c"),
          Node(7, "B", "b", Seq(
            Node(8, "B1", "b1a"))),
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2", Seq(
              Node(5, "A2a", "a2a")))))))

        val ops = EditScript.generate(t1, t2)
        ops shouldBe Seq(
          Move(7, 1, 0),
          Insert(Node(9, "C", "c"), 1, 0),
          Update(8, "b1a"),
          Delete(6))
      }

      it("should handle completely unmatching trees") {
        val t1 = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2", Seq(
              Node(5, "A2a", "a2a"))),
            Node(6, "A3", "a3"))),
          Node(7, "B", "b", Seq(
            Node(8, "B1", "b1")))))

        val t2 = Node(1, "ROOT", "", Seq(
          Node(9, "C", "c"),
          Node(7, "D", "d", Seq(
            Node(8, "D1", "d1a"))),
          Node(2, "E", "e", Seq(
            Node(3, "E1", "e1"),
            Node(4, "E2", "e2", Seq(
              Node(5, "E2a", "e2a")))))))

        val ops = EditScript.generate(t1, t2)
        (ops.foldLeft(t1) {(acc, op) => op apply acc}).isIsomorphicTo(t2) shouldBe true
      }
    }
  }
}
