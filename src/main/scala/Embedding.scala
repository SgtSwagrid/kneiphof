package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}

/**
  * Evidence that multiplicities in [[N]] can be read as multiplicities in
  * [[M]], preserving sums.
  *
  * Every multiplicity embeds in itself, and counts embed in every multiplicity
  * as repeated sums of `one`. This is what lets edges and chains, which are
  * multigraphs, be added to graphs of any kind, as in `g + (1 ~ 2)`.
  */
trait Embedding[N, M]:

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
    : IncidenceStructure[V, M, X] = Support(structure, this)

/** The companion object for [[Embedding]]. */
object Embedding extends CountEmbeddings:

  /** Every multiplicity embeds in itself. */
  given identity: [M] => Embedding[M, M]:

    override def apply(multiplicity: M): M = multiplicity
    override def exact: Boolean            = true

    override private[kneiphof] def embedded[V, X <: AnyEdge[?, ?, ?]]
      (structure: IncidenceStructure[V, M, X])
      (using Multiplicity[M])
      : IncidenceStructure[V, M, X] = structure
