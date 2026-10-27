package com.alecdorrington
package kneiphof
package incidence

/**
  * The sum of the graphs whose incidence structures are [[first]] and
  * [[second]], which adds the multiplicities of their edges. It's only used for
  * additive multiplicities, so its sizes and degrees are those of the two
  * graphs added.
  */
private[kneiphof] final class Sum[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    first: IncidenceStructure[V, M, X],
    second: IncidenceStructure[V, M, X],
  )
  extends Overlay[V, M, X](first, second):

  override protected def combine(x: M, y: M): M = x + y

  override lazy val size: Int = Math.addExact(first.size, second.size)

  override def degree(vertex: Any): Int = Math.addExact(
    first.degree(vertex),
    second.degree(vertex),
  )
