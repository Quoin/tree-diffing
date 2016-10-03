package com.quoininc.treediffing

import scala.collection.immutable.{Seq,Set}
import math.max



/*
 * This is based on the paper:
 * S.S. Chawathe, A. Rajaraman, H. Garcia-Molina, and J. Widom,
 * “Change Detection in Hierarchically Structured Information,”
 * Proc. ACM Sigmod Int’l Conf. Management of Data, pp. 493-504, June
 * 1996.
 *
 * Many of the variables are named after the variables used in that paper.
 * Comments starting with "|" indicate lines from the algorithm spec.
 */
object EditScript {
  import TreeMatcher.{Left,Right,MatchSet,MatchSetEnhancer}
  import Node._

  private def findPosition(
      node /* x in T2 */: Node,
      parent /* y in T2 */: Option[Node],
      t1: Node,
      matchSet: MatchSet,
      t1Ordered: Set[NodeId],
      t2Ordered: Set[NodeId]): Int = {

    (parent flatMap { y =>
      val orderedChildren = y.children filter { t2Ordered contains _.id }

      val nodesOnLeft = y.children.slice(0, y.children.indexOf(node))

      nodesOnLeft.
        reverse.
        find(t2Ordered contains _.id).
        flatMap { t2RightmostLeft /* v */ =>
          matchSet.findMatchOfRightNode(y, t1) flatMap { t1Parent =>
            matchSet.findMatchOfRightNode(t2RightmostLeft, t1).flatMap { t1RightmostLeft /* u */ =>
              val t1OrderedSiblings = t1Parent.children.filter(t1Ordered contains _.id)

              Some(t1OrderedSiblings.indexOf(t1RightmostLeft) + 1)
            }
          }
        }
    }).getOrElse(0)
  }

  private def alignChildren(
      n1: Node /* w in t1 */,
      n2: Node /* x in t2 */,
      t1: Node,
      matchSet: MatchSet /* M' */,
      originalMatchSet: MatchSet /* M */,
      t1Ordered: Set[NodeId],
      t2Ordered: Set[NodeId]
      ): (Node /* updated n1 */,
          Seq[Move],
          Set[NodeId] /* t1Ordered */,
          Set[NodeId] /* t2Ordered */) = {
    val s1 = n1.children.filter { matchSet.findMatchTuple(_, Left) match {
        case Some(m) => n2.children.map(_.id).contains(m._2)
        case None => false
      }
    }

    val s2 = n2.children.filter { matchSet.findMatchTuple(_, Right) match {
        case Some(m) => n1.children.map(_.id).contains(m._1)
        case None => false
      }
    }

    val s = lcs(s1, s2, matchSet)
    val childrenIds = (_: Node).children.map(_.id)

    (for { a <- s1; b <- s2 } yield (a, b)).filter({ m =>
      originalMatchSet.contains((m._1.id, m._2.id)) && !s.contains(m)
    }).foldLeft((
      n1,
      Seq[Move](),
      s.foldLeft(t1Ordered.filter(!childrenIds(n1).contains(_))) { _ + _._1.id },
      s.foldLeft(t2Ordered.filter(!childrenIds(n2).contains(_))) { _ + _._2.id })) {
        case ((parent, moves, t1OrderedAcc, t2OrderedAcc), (a, b)) =>
          val newMove = Move(
            a.id, parent.id, findPosition(b, Some(n2), t1, matchSet, t1OrderedAcc, t2OrderedAcc))
          (newMove apply parent, moves :+ newMove, t1OrderedAcc + a.id, t2OrderedAcc + b.id)
      }
  }

  /*
   * This is a recursive and not very efficient LCS algorithm (O(2^n)) but it
   * should work fine as long as most nodes have no more than a few children
   */
  def lcs(s1: Seq[Node], s2: Seq[Node], matchSet: MatchSet): Seq[(Node, Node)] =
    if (s1.isEmpty || s2.isEmpty)
      Seq[(Node, Node)]()
    else if (matchSet contains (s1.head.id, s2.head.id))
      (s1.head, s2.head) +: lcs(s1.tail, s2.tail, matchSet)
    else
      Seq(lcs(s1, s2.tail, matchSet), lcs(s1.tail, s2, matchSet)).maxBy (_.size)

  /*
   * 2(b) in the paper
   * Here we add the new node and its partner in T2 to the ordered sets so that
   * nodes get inserted correctly in the alignChildren function.  Otherwise the
   * nodes can be added in reverse order and result in non-equal trees.  The
   * paper's restricted definition of isomorphic trees seems to require this
   * ("We say that two trees are isomorphic if they are identical except for
   * node identifiers").
   */
   private def insertUnmatchedNode(
      n2 /* x */: Node,
      t1: Node,
      matchSet: MatchSet,
      n2Parent: Node,
      n2Pos: Int,
      t1Ordered: Set[NodeId],
      t2Ordered: Set[NodeId]): (
        Node /* new t1 */,
        MatchSet,
        Seq[EditOperation],
        Set[NodeId] /* new t1Ordered */,
        Set[NodeId] /* new t2Ordered */) = {
    // | Let z be the partner of y in M'
    // The parent must always be matched already because we are processing the
    // trees breadth-first, so if the below `get` throws an exception we have a
    // bug in the implementation
    val n2ParentPartner /* z */ = matchSet.findMatchOfRightNode(n2Parent, t1).get
    // | k = FindPos(x)
    // | Append INS((w, a, v(x)), z, k) to E, for a new identifier w
    // | Add (w, x) to M' and apply INS((w, a, v(x)), z, k) to T1
    val newNode = Node(t1.nextId, n2.label, n2.value)
    val op = Insert(newNode, n2ParentPartner.id, n2Pos)
    (op apply t1,
      matchSet + ((newNode.id, n2.id)),
      Seq(op),
      t1Ordered + newNode.id,
      t2Ordered + n2.id)
  }

  private def updateMatchedNode(
      n2 /* x */: Node,
      n2Partner /* w */: Node,
      t1: Node,
      matchSet: MatchSet,
      n2Parent /* y */: Node,
      n2Pos: Int): (Node /* new t1 */,
                    MatchSet,
                    Seq[EditOperation]) = {
    // | let v = p(w) in T1
    val partnerParent /* v in T1 */ = n2Partner.parent(t1).
      getOrElse { throw new AssertionError("v = p(w) in T1 is null") }

    def updateIfChanged(t1Acc: Node, ops: Seq[EditOperation]) =
      // | If v(w) != v(x)
      if (n2.value != n2Partner.value) {
        val updateOp = Update(n2Partner.id, n2.value)
        (updateOp apply t1Acc, ops :+ updateOp)
      } else (t1Acc, ops)

    def moveIfDifferentParents(t1Acc: Node, ops: Seq[EditOperation]) =
      // | If (y, v) ∉ M'
      if (!matchSet.contains((n2Partner.parent(t1).get.id, n2Parent.id))) {
        val n2ParentPartner /* z */ = matchSet.findMatchOfRightNode(n2Parent, t1).get
        val moveOp = Move(n2Partner.id, n2ParentPartner.id, n2Pos)
        (moveOp apply t1Acc, matchSet, ops :+ moveOp)
      } else (t1Acc, matchSet, ops)

    (moveIfDifferentParents _).tupled(updateIfChanged(t1, Seq()))
  }

  private def findChanges(
      n2 /* x */: Node,
      n2ParentOpt /* y */: Option[Node],
      t1: Node,
      matchSet: MatchSet,
      t1Ordered: Set[NodeId],
      t2Ordered: Set[NodeId]
      ): (Node /* new t1 */,
          MatchSet,
          Seq[EditOperation],
          Set[NodeId] /* new t1Ordered */,
          Set[NodeId] /* new t2Ordered */
          ) = {
    (n2ParentOpt map { n2Parent /* y */ =>
      val pos /* k */ = findPosition(n2, Some(n2Parent), t1, matchSet, t1Ordered, t2Ordered)
      matchSet.findMatchOfRightNode(n2, t1) match {
        // | (b) If x has no partner in M'
        case None =>
          insertUnmatchedNode(n2, t1, matchSet, n2Parent, pos, t1Ordered, t2Ordered)
        // | (c) else if x is not the root (x has a partner in M')
        case Some(partner /* w */) => {
          val (newT1, newMatchSet, ops) = updateMatchedNode(n2, partner, t1, matchSet, n2Parent, pos)
          (newT1, newMatchSet, ops, t1Ordered, t2Ordered)
        }
      }
    }).getOrElse ((t1, matchSet, Seq[EditOperation](), t1Ordered, t2Ordered))
  }

  private def findDeletes(
      t1: Node,
      updatedMatchSet /* M' */: MatchSet): (Seq[Delete], Node /* new T1 */) = {
    t1.postOrder.foldLeft((Seq[Delete](), t1)) { case ((ops, t1Acc), n1 /* w */) =>
      if (updatedMatchSet.isLeftNodeMatched(n1)) {
        (ops, t1Acc)
      } else {
        val newDel = Delete(n1.id)
        (ops :+ newDel, newDel apply t1Acc)
      }
    }
  }

  private def doMatch(t1: Node, t2: Node) = {
    val matchSet = TreeMatcher.matchTrees(t1, t2)

    // "If the roots of T1 and T2 are not matched in M , then we add
    // new dummy roots that are matched." - p.498, footnote 4
    /*if (!matchSet.contains((t1.id, t2.id))) {
      val dummyId = max(t1.nextId, t2.nextId)

      (Node(dummyId, "DUMMY", "", Seq(t1)),
        Node(dummyId, "DUMMY", "", Seq(t2)),
        matchSet + ((dummyId, dummyId)))
    } else */

    /* Just assume the root nodes always match.  This could cause issues if the
     * root of one tree was matched to an inner node of another tree, but the
     * dummy node technique seems to break things worse.
     */
      (t1, t2,
        if (matchSet.contains((t1.id, t2.id))) matchSet else matchSet + ((t1.id, t2.id)))
  }


  def generate(t1: Node, t2: Node): Seq[EditOperation] = {
    val (t1Init, t2Init, matchSet) = doMatch(t1, t2)

    val (editOps, updatedT1, finalMatchSet, _, _) = t2Init.breadthFirst.
      foldLeft((Seq[EditOperation](), t1Init, matchSet, Set[NodeId](), Set[NodeId]())) {
      case ((opsAcc, t1Acc, matchSetAcc, t1Ordered, t2Ordered), n2 /* x */) =>
        val (newT1, newMatchSet, ops, t1OrderedAcc, t2OrderedAcc) =
          findChanges(n2, n2.parent(t2Init), t1Acc, matchSetAcc, t1Ordered, t2Ordered)
        // n2 must have partner if our algorithm is correctly implemented
        val n2Partner = newMatchSet.findMatchOfRightNode(n2, newT1).get
        val (newN2Partner, moves, newT1Ordered, newT2Ordered) = alignChildren(
          n2Partner,
          n2,
          newT1,
          newMatchSet,
          matchSet,
          t1OrderedAcc,
          t2OrderedAcc)
        (opsAcc ++ ops ++ moves, newT1.replaceNode(n2Partner.id, newN2Partner), newMatchSet, newT1Ordered, newT2Ordered)
      }

    val (deleteOps, finalT1) = findDeletes(updatedT1, finalMatchSet)
    val allOps = editOps ++ deleteOps

    // If they aren't equal, our implementation is buggy
    assert(finalT1 isIsomorphicTo t2Init,
      s"Trees must be isomorphic:\n  t1=$finalT1\n  t2=$t2Init\n  ops: $allOps")
    assert(finalMatchSet.size == t2Init.postOrder.size,
      s"All nodes in T2 must be matched:\n  M': $finalMatchSet")

    allOps
  }
}


