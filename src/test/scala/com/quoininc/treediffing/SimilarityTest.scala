package com.quoininc.treediffing


import org.scalatest._

class SimilaritySpec extends FunSpec with Matchers {
  describe("bigramSim") {
    // I'm almost certain the CHANGE DISTILLING paper's example and the
    // author's Java implementation of the Dice coefficient is incorrect.  They
    // appear to use sets instead of "bags" and don't count duplicate bigrams
    // like the paper they quote as [1] says. There is one more bigram than
    // they calculated (15 replaces 14 in the numerator in their example near
    // the end of page 730)
    it("should compute example from paper correctly") {
      val sim = Similarity.bigramSim("verticalDrawAction", "drawVerticalAction")
      sim shouldBe (0.88f +- 0.01f)
    }

    it("should return 1 for identical strings") {
      val sim = Similarity.bigramSim("testing", "testing")
      sim shouldBe 1.0
    }

    it("should return 1 for two blank strings") {
      Similarity.bigramSim("", "") shouldBe 1.0
    }
  }
}
