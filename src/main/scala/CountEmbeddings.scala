package com.alecdorrington
package kneiphof

/** The embeddings of counts, which apply when no identity embedding does. */
trait CountEmbeddings:

  /** Counts embed in every multiplicity, as repeated sums of `one`. */
  given counts: [M : Multiplicity as M] => Embedding[Count, M]:

    override def apply(count: Count): M = M.fromInt(count.toInt)
    override def exact: Boolean         = false
