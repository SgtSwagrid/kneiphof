package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}
import scala.util.NotGiven

/**
  * Evidence that multiplicities in [[N]] can be read as multiplicities in
  * [[M]], preserving sums.
  *
  * Every multiplicity embeds in itself, and counts embed in every multiplicity
  * as repeated sums of `one`. This is what lets edges and chains, which are
  * multigraphs, be added to graphs of any kind, as in `g + (1 ~ 2)`.
  *
  * There are no others, so edge sets don't embed in multigraphs: reading each
  * edge as one copy doesn't preserve sums. Use [[HypergraphBase.counted]] or
  * [[DirectedHypergraphBase.counted]] to read an edge set as a multigraph
  * explicitly.
  */
sealed trait Embedding[N, M]:

  /** @return The multiplicity in [[M]] which [[multiplicity]] stands for. */
  def apply(multiplicity: N): M

  /**
    * `true` if and only if this embedding keeps the number of copies of every
    * edge, so that sizes and degrees are unchanged.
    */
  def exact: Boolean

  /**
    * @return
    *   The incidence structure of [[structure]] with its multiplicities read in
    *   [[M]] by this embedding.
    */
  private[kneiphof] def embedded[V, X <: AnyEdge[?, ?, ?]]
    (structure: IncidenceStructure[V, N, X])
    (using Multiplicity[M])
    : IncidenceStructure[V, M, X] = Support(structure, apply, exact)

/** The companion object for [[Embedding]]. */
object Embedding:

  /** Every multiplicity embeds in itself. */
  given identity: [M] => Embedding[M, M]:

    override def apply(multiplicity: M): M = multiplicity
    override def exact: Boolean            = true

    override private[kneiphof] def embedded[V, X <: AnyEdge[?, ?, ?]]
      (structure: IncidenceStructure[V, M, X])
      (using Multiplicity[M])
      : IncidenceStructure[V, M, X] = structure

  /** Counts embed in every other multiplicity, as repeated sums of `one`. */
  given counts
    : [M : Multiplicity as M] => NotGiven[M =:= Count] => Embedding[Count, M]:

    override def apply(count: Count): M = M.fromInt(count.toInt)
    override def exact: Boolean         = false
