package com.alecdorrington
package kneiphof

/**
  * A natural number of copies of an edge, being the multiplicities of a
  * multigraph.
  *
  * At runtime this is a plain [[Int]], but it can never be negative.
  */
opaque type Count = Int

/** The companion object for [[Count]]. */
object Count:

  /**
    * @return
    *   The count [[n]].
    * @throws IllegalArgumentException
    *   If `n < 0`.
    */
  def apply(n: Int): Count =
    require(
      n >= 0,
      s"A count must be non-negative, but was $n.",
    )
    n

  extension (count: Count)

    /** @return This count as an [[Int]]. */
    def toInt: Int = count

  /**
    * Multigraph multiplicities, where union and intersection take the maximum
    * and minimum.
    */
  given Multiplicity[Count]:

    override def zero: Count       = 0
    override def one: Count        = 1
    override def additive: Boolean = true

    extension (x: Count)

      override def add(y: Count): Count   = Math.addExact(x, y)
      override def mul(y: Count): Count   = Math.multiplyExact(x, y)
      override def or(y: Count): Count    = x.max(y)
      override def and(y: Count): Count   = x.min(y)
      override def monus(y: Count): Count = (x - y).max(0)
      override def count: Int             = x
