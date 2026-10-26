package com.alecdorrington
package kneiphof

import com.alecdorrington.scalgebra.arithmetic.Semiring
import com.alecdorrington.scalgebra.lattice.Lattice

/**
  * A typeclass for edge multiplicities, i.e. how many times a graph contains
  * each edge.
  *
  * A graph assigns each edge value a multiplicity, where [[zero]] means that
  * the edge is absent. Every graph operation is defined once in terms of this
  * algebra:
  *   - Sum (`+`) adds multiplicities.
  *   - Union (`|`) and intersection (`&`) take their join and meet.
  *   - Difference (`-`) subtracts them, stopping at [[zero]].
  *
  * With [[Boolean]] this gives graphs whose edges form a set, and with
  * [[Count]] it gives multigraphs.
  *
  * @note
  *   Laws, in addition to those inherited:
  *   - Order: `x | y == y` if and only if `x + z == y` for some `z`.
  *   - Bottom: `zero | x == x`.
  *   - Monus: `x - y` is the least `z` such that `x | (y + z) == y + z`.
  *   - Count: `zero.count == 0` and `one.count == 1`.
  *   - Additivity: if [[additive]], then `(x + y).count == x.count + y.count`.
  */
trait Multiplicity[M] extends Semiring[M], Lattice[M]:

  /**
    * `true` if and only if sums add the numbers of copies, as for [[Count]] but
    * not [[Boolean]], so that sizes and degrees add too.
    */
  def additive: Boolean

  extension (x: M)

    /**
      * Computes the truncated difference of [[x]] and [[y]], i.e. `x - y`,
      * stopping at [[zero]].
      */
    def monus(y: M): M

    /** An alias for [[monus]]. */
    inline infix def - (y: M): M = x.monus(y)

    /** The number of copies of an edge that [[x]] stands for. */
    def count: Int

/** The companion object for [[Multiplicity]]. */
object Multiplicity:

  /** Set multiplicities, where each edge is either present or absent. */
  given Multiplicity[Boolean]:

    override def zero: Boolean     = false
    override def one: Boolean      = true
    override def additive: Boolean = false

    extension (x: Boolean)

      override def add(y: Boolean): Boolean   = x || y
      override def mul(y: Boolean): Boolean   = x && y
      override def or(y: Boolean): Boolean    = x || y
      override def and(y: Boolean): Boolean   = x && y
      override def monus(y: Boolean): Boolean = x && !y
      override def count: Int                 = if x then 1 else 0
