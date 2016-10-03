package com.quoininc.treediffing

import scala.collection.immutable.{Set,Map,HashSet}
import Math.{min,max}


object TreeMatcher {
  import Node._
  type MatchSet = Set[(NodeId, NodeId)]

  val ValueMatchThreshold = 0.6
  val ShortValueMatchThreshold = 0.5
  val NormalInnerThreshold = 0.6
  val SmallInnerThreshold = 0.4

  import Similarity.bigramSim

  case class MatchedLeaf(l1: Node, l2: Node, similarity: Float)

  sealed abstract class LeftOrRight(index: Int)
  case object Left extends LeftOrRight(1)
  case object Right extends LeftOrRight(2)

  implicit class MatchSetEnhancer(matchSet: MatchSet) {
    def findMatchTuple(node: Node, sideToSearch: LeftOrRight): Option[(NodeId, NodeId)] =
      matchSet find { t => (if (sideToSearch == Left) t._1 else t._2) == node.id }

    def findMatchOfLeftNode(node: Node, t2: Node): Option[Node] =
      findMatchTuple(node, Left) map (t2 getById _._2)
    def findMatchOfRightNode(node: Node, t1: Node): Option[Node] =
      findMatchTuple(node, Right) map (t1 getById _._1)
    def isLeftNodeMatched(node: Node): Boolean = (findMatchTuple(node, Left)).isDefined
    def isRightNodeMatched(node: Node): Boolean = (findMatchTuple(node, Right)).isDefined
  }

  private def matchLeaves(t1: Node, t2: Node) = {
    val matching = for {
        l1 <- t1.leaves
        l2 <- t2.leaves
        sim = bigramSim(l1.value, l2.value)
        if (doLeavesMatch(l1, l2, sim))
      } yield MatchedLeaf(l1, l2, sim)

    (matching.toSeq sortBy {- _.similarity} foldLeft(Set[(NodeId, NodeId)]() /* == MatchSet() */)) {
      (matches, proposedMatch) =>
        if (matches exists { case (n1, n2) => n1 == proposedMatch.l1.id || n2 == proposedMatch.l2.id })
          matches
        else
          matches + ((proposedMatch.l1.id, proposedMatch.l2.id))
    }
  }

  private def doLeavesMatch(l1: Node, l2: Node, sim: Float): Boolean =
    l1.label == l2.label &&
      sim >= dynamicValueThreshold(l1.value, l2.value)

  // This is not in the paper's algorithm but it seems to help with short
  // values
  private def dynamicValueThreshold(value1: String, value2: String) =
    if (value1.size <= 3 || value2.size <= 3)
      ShortValueMatchThreshold
    else
      ValueMatchThreshold

  private def doInnerNodesMatch(n1: Node, n2: Node, matchSet: MatchSet): Boolean =
    n1.label == n2.label &&
      commonLeafCount(n1, n2, matchSet).toFloat / max(n1.numLeaves, n2.numLeaves) >=
        dynamicInnerThreshold(n1, n2) &&
      bigramSim(n1.value, n2.value) >= ValueMatchThreshold

  private def dynamicInnerThreshold(n1: Node, n2: Node) =
    if (n1.leaves.size <= 4 && n2.leaves.size <= 4)
      SmallInnerThreshold
    else
      NormalInnerThreshold

  private def commonLeafCount(t1: Node, t2: Node, matchSet: MatchSet) =
    (for {
      l1 <- t1.leaves
      l2 <- t2.leaves
      if (matchSet contains (l1.id, l2.id))
    } yield l1).size

  def matchTrees(t1: Node, t2: Node): MatchSet = {
    val matchSet = matchLeaves(t1, t2)

    t1.postOrder.foldLeft(matchSet) { (acc, n1) =>
      if (acc.isLeftNodeMatched(n1)) acc
      else {
        val postOrderUnmatchedT2 = t2.postOrder filter (!acc.isRightNodeMatched(_))
        val potentialMatch = postOrderUnmatchedT2 find (doInnerNodesMatch(n1, _, acc))

        potentialMatch map (n2 => acc + ((n1.id, n2.id))) getOrElse acc
      }
    }
  }

  implicit class NodeWithMatching(node: Node) {
    def matchWith(t2: Node): MatchSet = TreeMatcher.matchTrees(node, t2)
  }

}
