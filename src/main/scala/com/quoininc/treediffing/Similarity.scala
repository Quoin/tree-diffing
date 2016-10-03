package com.quoininc.treediffing


object Similarity {
  // N.B. n-grams are calculated case insensitive
  def nGrams(n: Int, s: String) = (s.toLowerCase sliding(n) map (_.mkString)).toSeq

  def bigramSim(s1: String, s2: String) =
    if (s1 == s2)
      1
    else
      diceCoefficient(nGrams(2, s1), nGrams(2, s2))

  def diceCoefficient(ngrams1: Seq[String], ngrams2: Seq[String]) =
    2.toFloat * (ngrams1 intersect ngrams2).size / (ngrams1.size + ngrams2.size)
}
