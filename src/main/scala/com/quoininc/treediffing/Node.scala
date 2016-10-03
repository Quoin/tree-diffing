package com.quoininc.treediffing

import scala.collection.immutable.{Seq, Queue}


case class Node(id: Int, label: String, value: String, children: Seq[Node] = Seq[Node]()) {
  import Node.{NodeId}
  private def _breadthFirst(queue: Queue[Node]): Seq[Node] = {
    if (queue.isEmpty) {
      Seq()
    } else {
      val (node, rest) = queue.dequeue
      Seq(node) ++ _breadthFirst(rest.enqueue(node.children))
    }
  }
  def breadthFirst: Seq[Node] = _breadthFirst(Queue[Node](this))

  private def _postOrder(acc: Seq[Node] = Seq[Node]()): Seq[Node] =
    if (isLeaf) acc ++ Seq(this) else children.foldLeft(acc) { _ ++ _._postOrder() } :+ this
  def postOrder: Seq[Node] = _postOrder()

  def parent(root: Node): Option[Node] = root.breadthFirst.find { _.children contains this }

  def isLeaf: Boolean = children.isEmpty

  def leaves: Set[Node] = if (isLeaf) Set(this) else children.foldLeft(Set[Node]()) (_ ++ _.leaves)

  def numLeaves: Int = leaves.size

  def orderOfChild(child: Node): Int = children.indexOf(child)

  def replaceNode(nodeId: Int, newNode: Node): Node = {
    def recurse(curNode: Node): Node =
      if (curNode.id == nodeId) {
        newNode
      } else {
        curNode.copy(children = curNode.children.map(recurse(_)).filter(_ != null))
      }
    recurse(this)
  }

  def findById(id: NodeId): Option[Node] = postOrder.find(_.id == id)
  def getById(id: NodeId): Node = findById(id).get

  def nextId: Int = postOrder.map(_.id).max + 1

  /*
   * "We say that two trees are isomorphic if they are identical except for
   * node identifiers." - CHANGE DETECTION, p.496
   *
   * This is different from other definitions of isomorphism that allow for
   * mirror images of sub-trees.
   */
  def isIsomorphicTo(other: Node): Boolean =
    if(label != other.label || value != other.value || children.size != other.children.size)
      false
    else if (children.size > 0)
      children.zip(other.children).forall { t => t._1 isIsomorphicTo t._2 }
    else
      true
}

object Node {
  type NodeId = Int

  sealed trait EditOperation {
    def apply(t: Node): Node
  }

  case class Update(nodeId: NodeId, newValue: String) extends EditOperation {
    def apply(t: Node) = t.replaceNode(nodeId, t.getById(nodeId).copy(value=newValue))
  }

  case class Delete(nodeId: NodeId) extends EditOperation {
    def apply(t: Node) = t.replaceNode(nodeId, null)
  }

  case class Insert(newNode: Node, parentId: NodeId, position: Int) extends EditOperation {
    def apply(t: Node) = {
      assert(!t.findById(newNode.id).isDefined, "New node to insert should have unique id")
      val oldParent = t.getById(parentId)
      val newParent = oldParent.copy(
        children = oldParent.children.take(position) ++ Seq(newNode) ++ oldParent.children.drop(position))

      t.replaceNode(parentId, newParent)
    }
  }

  case class Move(nodeId: NodeId, parentId: NodeId, position: Int) extends EditOperation {
    def apply(t: Node) = {
      val node = t.getById(nodeId)
      Insert(node, parentId, position).apply(Delete(nodeId).apply(t))
    }
  }
}
