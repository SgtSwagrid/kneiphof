package com.alecdorrington
package kneiphof

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

/** The companion object for [[Embedding]]. */
object Embedding extends CountEmbeddings:

  /** Every multiplicity embeds in itself. */
  given identity: [M] => Embedding[M, M]:

    override def apply(multiplicity: M): M = multiplicity
    override def exact: Boolean            = true

/** The embeddings of counts, which apply when no identity embedding does. */
trait CountEmbeddings:

  /** Counts embed in every multiplicity, as repeated sums of `one`. */
  given counts: [M : Multiplicity as M] => Embedding[Count, M]:

    override def apply(count: Count): M = M.fromInt(count.toInt)
    override def exact: Boolean         = false
