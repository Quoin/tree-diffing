package com.quoininc.treediffing

import org.scalatest.FunSpec

import scala.collection.immutable.Seq

class NodeSpec extends FunSpec {

  describe("A Node") {
    describe("breadthFirst traversal") {
      it("should include only itself if a leaf node") {
        val root = Node(1, "LABEL", "VALUE")
        assert(root.breadthFirst == Seq(root))
      }

      it("should be correctly ordered for three level tree") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b")))

        assert(root.breadthFirst.map (_.id) == Seq(1, 2, 5, 3, 4))
      }
    }
    describe("postOrder traversal") {
      it("should include only itself if a leaf node") {
        val root = Node(1, "LABEL", "VALUE")
        assert(root.postOrder == Seq(root))
      }

      it("should be correctly ordered for three level tree") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.postOrder.map (_.id) == Seq(3, 4, 2, 7, 8, 6, 5, 1))
      }
    }

    describe("parent") {
      it("should return Option(None) if root") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a")))

        assert(root.parent(root) == None)
      }

      it("should find the parent if non-root") {
        val child = Node(2, "A", "a")
        val root = Node(1, "ROOT", "", Seq(child))

        assert(child.parent(root) == Some(root))
      }

      it("should find the parent deeply nested") {
        val child = Node(4, "A1A", "a1a")
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1", Seq(child))))))

        assert(child.parent(root).get.id == 3)
      }
    }

    describe("leaves") {
      it("should return only leaf nodes") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.leaves.map(_.id).toSet == Set(3,4,7,8))
      }
    }

    describe("replaceNode") {
      it("should replace same node passed in") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        val newRoot = Node(1, "ROOT", "", Seq(
          Node(2, "Z", "z", Seq(
            Node(3, "Z1", "z1"),
            Node(4, "Z2", "z2"))),
          Node(5, "Y", "y", Seq(
            Node(6, "Y1", "y1", Seq(
              Node(7, "YA", "ya"),
              Node(8, "Yy", "bb")))))))

        assert(root.replaceNode(root.id, newRoot) == newRoot)
      }

      it("should replace leaf node") {
        val originalNode = Node(4, "A2", "a2")

        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            originalNode)),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        val newNode = Node(9, "A3", "a3", Seq(
          Node(10, "AA3", "aa3")))

        val expected = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            newNode)),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.replaceNode(originalNode.id, newNode) == expected)
      }

      it("should replace inner node") {
        val originalNode = Node(4, "A2", "a2", Seq(
          Node(11, "LEAF", "leaf")))

        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            originalNode)),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        val newNode = Node(9, "A3", "a3")

        val expected = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            newNode)),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.replaceNode(originalNode.id, newNode) == expected)
      }

      it("should remove node when newNode is null") {
        val originalNode = Node(4, "A2", "a2", Seq(
          Node(11, "LEAF", "leaf")))

        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            originalNode)),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        val expected = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"))),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.replaceNode(originalNode.id, null) == expected)
      }
    }

    describe("Update Operation") {
      import Node.Update

      it("should update value only") {
        val nodeToUpdate = Node(2, "A", "a")
        val root = Node(1, "ROOT", "", Seq(
          nodeToUpdate))

        val newRoot = Update(nodeToUpdate.id, "new").apply(root)
        assert(newRoot == Node(1, "ROOT", "", Seq(
          Node(2, "A", "new"))))
      }
    }

    describe("Insert Operation") {
      import Node.Insert

      it("should insert to root node at end") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a")))

        val newRoot = Insert(Node(5, "B", "b"), root.id, 1).apply(root)
        assert(newRoot == Node(1, "ROOT", "", Seq(
          Node(2, "A", "a"),
          Node(5, "B", "b"))))
      }

      it("should insert to inner node at beginning") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "B", "b")))))

        val newRoot = Insert(Node(5, "C", "c"), root.children(0).id, 0).apply(root)
        assert(newRoot == Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(5, "C", "c"),
            Node(3, "B", "b"))))))
      }
    }

    describe("Delete Operation") {
      import Node.Delete

      it("should delete leaf node") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "B", "b")))))

        val newRoot = Delete(3) apply root
        assert(newRoot == Node(1, "ROOT", "", Seq(
          Node(2, "A", "a"))))
      }
    }

    describe("Move Operation") {
      import Node.Move

      it("should move within same parent") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a"),
          Node(3, "B", "b", Seq(
            Node(4, "B1", "b1")))))

        val newRoot = Move(3, 1, 0) apply root

        assert(newRoot == Node(1, "ROOT", "", Seq(
          Node(3, "B", "b", Seq(
            Node(4, "B1", "b1"))),
          Node(2, "A", "a"))))
      }
    }

    describe("nextId") {
      it("should return one higher than the highest id used") {
        val root = Node(1, "ROOT", "", Seq(
          Node(2, "A", "a", Seq(
            Node(3, "A1", "a1"),
            Node(4, "A2", "a2"))),
          Node(5, "B", "b", Seq(
            Node(6, "B1", "b1", Seq(
              Node(7, "BA", "ba"),
              Node(8, "BB", "bb")))))))

        assert(root.nextId == 9)
      }
    }
  }
}
